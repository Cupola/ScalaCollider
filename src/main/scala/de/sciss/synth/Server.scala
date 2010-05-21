/*
 *  Server.scala
 *  (ScalaCollider)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.synth

import de.sciss.scalaosc.{ OSCChannel, OSCClient, OSCMessage, OSCPacket }
import java.net.{ ConnectException, InetAddress, InetSocketAddress, SocketAddress }
import java.io.{ BufferedReader, File, InputStreamReader, IOException }
import java.util.{ Timer, TimerTask }
import actors.{ Actor, Channel, DaemonActor, Future, InputChannel, OutputChannel, TIMEOUT }
import collection.immutable.Queue
import concurrent.SyncVar
import osc._
import math._

/**
 * 	@version    0.16, 20-May-10
 */
object Server {
   private val allSync  = new AnyRef
//   private var allVar   = Set.empty[ Server ]
   var default: Server  = null

//   def all     = allVar

   private def add( s: Server ) {
      allSync.synchronized {
//         allVar += s
         if( default == null ) default = s
      }
   }

   private def remove( s: Server ) {
      allSync.synchronized {
//         allVar -= s
         if( default == s ) default = null
      }
   }

   def printError( name: String, t: Throwable ) {
      println( name + " : " )
      t.printStackTrace()
   }

   abstract sealed class Condition
   case object Running extends Condition
   case object Booting extends Condition
   case object Offline extends Condition
   private case object Terminating extends Condition
   private case object NoPending extends Condition

   case class Counts( c: OSCStatusReplyMessage )
}

//abstract class Server extends Model {}
class Server( val name: String = "localhost", val options: ServerOptions = new ServerOptions, val clientID: Int = 0 )
extends Model {
   import Server._

   private var aliveThread: Option[StatusWatcher]	= None
   private var bootThread: Option[BootThread]		= None
   private var countsVar							      = new OSCStatusReplyMessage( 0, 0, 0, 0, 0f, 0f, 0.0, 0.0 )
   private var collBootCompletion					   = Queue.empty[ (Server) => Unit ]
   private val condSync                            = new AnyRef
   private var conditionVar: Condition 			   = Offline
   private var pendingCondition: Condition      	= NoPending
//   private var bufferAllocatorVar: ContiguousBlockAllocator = null
   private val host                                = InetAddress.getByName( options.host.value )

   val addr                                        = new InetSocketAddress( host, options.port.value )
   def isLocal                                     = host.isLoopbackAddress || host.isSiteLocalAddress
   val rootNode                                    = new Group( this, 0 )
   val defaultGroup                                = new Group( this, 1 )
   val nodeMgr                                     = new NodeManager( this )
   val bufMgr                                      = new BufferManager( this )
   var latency                                     = 0.2f

   private val c                                   = {
      val client        = OSCClient( Symbol( options.protocol.value ), 0, host.isLoopbackAddress, ServerCodec )
      client.bufferSize = 0x10000
      client.target     = addr
      client.action     = OSCReceiverActor.messageReceived
      client
   }

   // ---- constructor ----
   {
      add( this )
//      createNewAllocators
//    resetBufferAutoInfo
   }

   // ---- abstract methods ----
//   protected def invokeOnMainThread( task: Runnable ) : Unit
//   val name: String
//   val options: ServerOptions
//   val clientID: Int
   
   def isConnected = c.isConnected
   def isRunning = condSync.synchronized { conditionVar == Running }
   def isBooting = condSync.synchronized { conditionVar == Booting }
   def isOffline = condSync.synchronized { conditionVar == Offline }
//   def bufferAllocator = bufferAllocatorVar

   object nodes {
      private var allocator : NodeIDAllocator = _
    
      reset
    
      def nextID = { allocator.alloc }
      def reset = { allocator = new NodeIDAllocator( clientID, options.initialNodeID )}
   }
  
   object busses {
      private var controlAllocator : ContiguousBlockAllocator = _
      private var audioAllocator : ContiguousBlockAllocator = _
    
      reset
    
      def reset = {
    	   controlAllocator	= new ContiguousBlockAllocator( options.controlBusChannels.value )
    	   audioAllocator		= new ContiguousBlockAllocator( options.audioBusChannels.value, options.firstPrivateBus )
      }
    
      def allocControl( numChannels: Int ) = controlAllocator.alloc( numChannels )
      def allocAudio( numChannels: Int ) = audioAllocator.alloc( numChannels )
      def freeControl( index: Int ) = controlAllocator.free( index )
      def freeAudio( index: Int ) = audioAllocator.free( index )
   }

   object buffers {
      private var allocator: ContiguousBlockAllocator = _

      reset

      def reset = {
         allocator = new ContiguousBlockAllocator( options.audioBuffers.value )
      }

      def alloc( numChannels: Int ) = allocator.alloc( numChannels )
      def free( index: Int ) = allocator.free( index )
   }

   def !( p: OSCPacket ) { c.send( p )}

   def !![ A ]( p: OSCPacket, handler: PartialFunction[ OSCMessage, A ]) : Future[ A ] = {
      val c    = new Channel[ A ]( Actor.self )
      val fun  = (res: SyncVar[ A ]) => {
         val futCh   = new Channel[ A ]( Actor.self )
         val oh      = new OSCInfHandler( handler, futCh )
         OSCReceiverActor.addHandler( oh )
         futCh.react { case r => res.set( r )}
      }
      val a = new FutureActor[ A ]( fun, c )
      a.start()
      this ! p
      a
   }

   def !?( timeOut: Long, p: OSCPacket, handler: PartialFunction[ Any, Unit ]) {
      val a = new DaemonActor {
         def act {
            val futCh   = new Channel[ Any ]( Actor.self )
            val oh      = new OSCTimeOutHandler( handler, futCh )
            OSCReceiverActor.addHandler( oh )
            futCh.reactWithin( timeOut ) {
               case TIMEOUT   => OSCReceiverActor.removeHandler( oh )
               case r         =>
            }
         }
      }
      a.start()
      this ! p
      a
   }


//   // XXX should use actor syntax instead, returning an Option[ OSCMessage ] to the current actor
//   private def sendAsyncPacket( ap: AsyncOSCPacket, timeOut: Long = 5000 )( success: => Unit )( failure: => Unit ) {
//      val sync = new AnyRef
//      var handled = false
//      val timer = new Timer( "TimeOut", true )
//      lazy val funSucc: (SocketAddress, Long) => Unit = (addr, when) => {
////println( "---4" )
//         timer.cancel
//         sync.synchronized {
//            multiResponder.removeListener( ap.replyMessage, funSucc )
//            if( !handled ) {
////println( "---5" )
//               handled = true
//               invokeOnMainThread( new Runnable {
//                  def run = {
////println( "---6" )
//                     success
//                  }
//               })
//            }
//         }
//      }
//      timer.schedule( new TimerTask {
//         def run = {
////println( "---1" )
//            invokeOnMainThread( new Runnable {
//               def run = sync.synchronized {
////println( "---2" )
//                  if( !handled ) {
////println( "---3" )
//                     multiResponder.removeListener( ap.replyMessage, funSucc )
//                     handled = true
//                     failure
//                  }
//               }
//            })
//         }
//      }, timeOut )
////println( "---0" )
//      multiResponder.addListener( ap.replyMessage, funSucc )
//      this ! ap.packet
//   }

   def counts = countsVar
   protected[synth] def counts_=( newCounts: OSCStatusReplyMessage ) {
      countsVar = newCounts
      dispatch( Counts( newCounts ))
   }

   def sampleRate = counts.sampleRate
  
   def dumpTree : Unit = dumpTree( false )

   def dumpTree( controls: Boolean ) {
      rootNode.dumpTree( controls )
   }
  
   def condition = condSync.synchronized { conditionVar }
   protected[synth] def condition_=( newCondition: Condition ) {
      condSync.synchronized {
         if( newCondition != conditionVar ) {
            conditionVar = newCondition
            if( newCondition == Offline ) {
               pendingCondition = NoPending
               serverLost
            } else if( newCondition == Running ) {
               if( pendingCondition == Booting ) {
                  pendingCondition = NoPending
                  collBootCompletion.foreach( action => try {
                        action.apply( this )
                     }
                     catch { case e => e.printStackTrace() }
                  )
                  collBootCompletion = Queue.empty
               }
            }
            dispatch( newCondition )
         }
      }
   }

   def startAliveThread( delay: Float = 0.25f, period: Float = 0.25f, deathBounces: Int = 25 ) {
      condSync.synchronized {
         if( aliveThread.isEmpty ) {
            val statusWatcher = new StatusWatcher( delay, period, deathBounces )
            aliveThread = Some( statusWatcher )
            statusWatcher.start
         }
      }
   }

   def stopAliveThread {
      condSync.synchronized {
         aliveThread.foreach( _.stop )
         aliveThread = None
      }
  }

   def queryCounts {
      this ! OSCStatusMessage
   }

   // note: this is called register instead of
   // notify to avoid conflict with notify in java.lang.Object
   def register : Unit = register( true )
   def register( onOff: Boolean ) {
      this ! notifyMsg( onOff )
   }

   def notifyMsg( onOff: Boolean = true ) = OSCServerNotifyMessage( onOff )

   def dumpOSC( mode: Int = OSCChannel.DUMP_TEXT ) {
      c.dumpIncomingOSC( mode, filter = {
         case m: OSCStatusReplyMessage => false
         case _ => true
      })
      c.dumpOutgoingOSC( mode, filter = {
         case OSCStatusMessage => false
         case _ => true
      })
   }

   def boot: Unit = boot( true )
   def boot( createAliveThread: Boolean ) {
      if( !isLocal ) error( "Server.boot : only allowed for local servers!" )

//      val whenBooted = (s: Server) => {
//         try {
////            println( "notification is on" )
//            s.register
//            s.initTree
//         }
//         catch { case e: IOException => printError( "Server.boot", e )}
//      }
//
      condSync.synchronized {
         if( pendingCondition != NoPending ) {
            println( "Server:boot - ongoing operations" )
            return
         }
         if( conditionVar == Running ) {
            println( "Server:boot - already running" )
            return
         }

         pendingCondition  = Booting
         condition         = Booting

         try {
//            createNewAllocators
// XXX resetBufferAutoInfo

//            addDoWhenBooted( whenBooted )
            if( bootThread.isEmpty ) {
               val thread	= new BootThread( createAliveThread )
               bootThread	= Some( thread )
               thread.start
            } else error( "Illegal state : boot thread still set" )
         }
         catch { case e => {
//            removeDoWhenBooted( whenBooted )
            try {
               stopAliveThread
            }
            catch { case e2 => printError( "Server.boot", e2 )}
            condition = Offline // pendingCondition = NoPending
            throw e
         }}
      }
   }

   private def serverContacted {
      createNewAllocators
//      this ! notifyMsg( true )
      this !? (5000L, notifyMsg( true ), {
         case OSCMessage( "/done", "/notify" ) => {
            initTree
            condition = Running
         }
      })
   }

   private def serverLost {
      nodeMgr.clear
      bufMgr.clear
      OSCReceiverActor.clear
   }

   private def bootThreadTerminated {
      condSync.synchronized {
         bootThread = None
         stopAliveThread
         condition = Offline
      }
   }
  
   // note: we do _not_ reset the nodeIDallocator here
   def initTree {
      nodeMgr.register( defaultGroup )
      this ! defaultGroup.newMsg( rootNode, addToHead )
   }
  
   def addDoWhenBooted( action: => Unit ) {
      val actionFunc = (s: Server) => action
      addDoWhenBooted( actionFunc )
   }

   def addDoWhenBooted( action: (Server) => Unit ) {
      condSync.synchronized {
         if( condition == Running ) {
            try {
               action.apply( this )
            }
            catch { case e => e.printStackTrace() }
         } else {
            collBootCompletion = collBootCompletion.enqueue( action )
         }
      }
   }
  
   def removeDoWhenBooted( action: (Server) => Unit ) {
      condSync.synchronized {
         collBootCompletion = collBootCompletion.filterNot( _ == action )
      }
   }
 
   private def createNewAllocators {
      nodes.reset
      busses.reset
      buffers.reset
   }

   def quit {
      this ! quitMsg
//      println( "/quit sent" )
      cleanUpAfterQuit
   }

   def quitMsg = OSCServerQuitMessage

   private def cleanUpAfterQuit {
      try {
         condSync.synchronized {
            stopAliveThread
            pendingCondition = Terminating
         }
      }
      catch { case e: IOException => printError( "Server.cleanUpAfterQuit", e )}
   }
  
   def startClient {
      OSCReceiverActor.start
      c.start
   }

   private[synth] def addResponder( resp: OSCResponder ) {
      OSCReceiverActor.addHandler( resp )
   }

   private[synth] def removeResponder( resp: OSCResponder ) {
      OSCReceiverActor.removeHandler( resp )
   }

//   protected[synth] def responderSync : AnyRef = multiResponder.nodeSync

   def dispose {
      condSync.synchronized {
         remove( this )
         c.dispose // = (msg: OSCMessage, sender: SocketAddress, time: Long) => ()
         OSCReceiverActor.dispose
//         c.dispose
      }
   }

   override def toString = "<" + name + ">"

   // -------- internal class BootThread -------- 

   private class BootThread( createAliveThread: Boolean )
   extends Thread {
      var keepRunning = true

      private val program     = options.programPath.value
//      println( "Booting '" + program + "'" )
      private val file        = new File( program )
      private val processArgs = options.toRealtimeArgs.toArray
      private val pb          = new ProcessBuilder( processArgs: _* )
         .directory( file.getParentFile )
         .redirectErrorStream( true )

      override def run {
         try {
            val p          = pb.start
            val inReader   = new BufferedReader( new InputStreamReader( p.getInputStream ))
            new Thread( new Runnable {
               def run {
                  while( true ) {
                     val line = inReader.readLine
                     if( line != null ) println( line ) else return
                  }
               }
            }).start

            // connect phase
            var cStarted   = false
            var pRunning   = true
            while( keepRunning && pRunning && !cStarted ) {
               try {
                  startClient
                  cStarted = true
                  // note that we optimistically assume that if we boot the server, it
                  // will not die (exhausting deathBounces). if it crashes, the boot
                  // thread's process will know anyway. this way we avoid stupid
                  // server offline notifications when using slow asynchronous commands
                  if( createAliveThread ) startAliveThread( 1.0f, 0.25f, Int.MaxValue )
               }
               catch { case e: ConnectException => Thread.sleep( 500 )} // thrown when in TCP mode and socket not yet available
               try {
                  p.exitValue // throws an exception if process still running
                  pRunning	= false
               }
               catch { case e: IllegalThreadStateException => } // gets thrown if we call exitValue() while sc still running
            }

            if( keepRunning && pRunning ) {
               p.waitFor()
            } else {
               p.destroy()
            }

            println( "scsynth terminated (" + p.exitValue +")" )
         }
         catch { case e: IOException => printError( "BootThread.run", e )} // thrown if process was not built
         finally {
//            invokeOnMainThread( new Runnable { def run = bootThreadTerminated })
            bootThreadTerminated
         }
      }
   }

   // -------- internal class StatusWatcher --------

   private class StatusWatcher( delay: Float, period: Float, deathBounces: Int )
   extends Runnable {
      watcher =>

      private var	alive			   = deathBounces
      private val	delayMillis		= (delay * 1000).toInt
      private val	periodMillis	= (period * 1000).toInt
//      private val	timer			   = new SwingTimer( periodMillis, this )
      private var timer: Option[ Timer ] = None
      private var callServerContacted  = true
      private val sync           = new AnyRef

//      // ---- constructor ----
//      timer.setInitialDelay( delayMillis )

      def start {
         stop
         timer = {
            val t = new Timer( "StatusWatcher", true )
            t.schedule( new TimerTask {
               def run = watcher.run // invokeOnMainThread( watcher )
            }, delayMillis, periodMillis )
            Some( t )
         }
      }

      def stop {
//         timer.stop
         timer.foreach( t => {
            t.cancel
            timer = None
         })
      }

      def run {
         sync.synchronized {
            alive -= 1
            if( alive < 0 ) {
               callServerContacted = true
               condition = Offline
            }
         }
         try {
            queryCounts
         }
         catch { case e: IOException => printError( "Server.status", e )}
      }

      def statusReply( msg: OSCStatusReplyMessage ) {
         sync.synchronized {
            alive = deathBounces
            // note: put the counts before running
            // because that way e.g. the sampleRate
            // is instantly available
            counts = msg
            if( !isRunning && callServerContacted ) {
               callServerContacted = false
               serverContacted
            }
         }
      }
   }

   object OSCReceiverActor extends DaemonActor {
      private case object Clear
      private case object Dispose
      private case class  ReceivedMessage( msg: OSCMessage, sender: SocketAddress, time: Long )
      private case class  AddHandler( h: OSCHandler )
      private case class  RemoveHandler( h: OSCHandler )

      def clear {
         this ! Clear
      }

      def dispose {
         clear
         this ! Dispose
      }

      def addHandler( handler: OSCHandler ) {
         this ! AddHandler( handler )
      }

      def removeHandler( handler: OSCHandler ) {
         this ! RemoveHandler( handler )
      }

      // ------------ OSCListener interface ------------

      def messageReceived( msg: OSCMessage, sender: SocketAddress, time: Long ) {
         this ! ReceivedMessage( msg, sender, time )
      }

      def act {
         var running    = true
         var handlers   = Set.empty[ OSCHandler ]
         loopWhile( running )( react {
            case ReceivedMessage( msg, sender, time ) => {
               msg match {
                  case nodeMsg:        OSCNodeChange           => nodeMgr.nodeChange( nodeMsg )
                  case bufInfoMsg:     OSCBufferInfoMessage    => bufMgr.bufferInfo( bufInfoMsg )
                  case statusReplyMsg: OSCStatusReplyMessage   => aliveThread.foreach( _.statusReply( statusReplyMsg ))
                  case _ =>
               }
               handlers.foreach( h => if( h.handle( msg )) handlers -= h )
            }
            case AddHandler( h )    => handlers += h
            case RemoveHandler( h ) => if( handlers.contains( h )) { handlers -= h; h.removed }
            case Clear              => { handlers.foreach( _.removed ); handlers = Set.empty }
            case Dispose            => running = false
            case m                  => println( "Received illegal message " + m )
         })
      }
   }

   // -------- internal OSCHandler implementations --------

   private class OSCInfHandler[ A ]( fun: PartialFunction[ OSCMessage, A ], ch: OutputChannel[ A ])
   extends OSCHandler {
      def handle( msg: OSCMessage ) : Boolean = {
         val handled = fun.isDefinedAt( msg )
         if( handled ) try {
            ch ! fun.apply( msg )
         } catch { case e => e.printStackTrace() }
         handled
      }
      def removed {}
   }

   private class OSCTimeOutHandler( fun: PartialFunction[ Any, Unit ], ch: OutputChannel[ Any ])
   extends OSCHandler {
      def handle( msg: OSCMessage ) : Boolean = {
         val handled = fun.isDefinedAt( msg )
         if( handled ) try {
            ch ! fun.apply( msg )
         } catch { case e => e.printStackTrace() }
         handled
      }
      def removed {
         if( fun.isDefinedAt( TIMEOUT )) try {
            fun.apply( TIMEOUT )
         } catch { case e => e.printStackTrace() }
      }
   }

   // -------- internal class FutureActor --------

   /*
    *    FutureActor in scala.actors is not very accessible...
    *    We need our own implementation of Future is seems
    */
   private class FutureActor[ T ]( fun: SyncVar[ T ] => Unit, channel: Channel[ T ])
   extends Future[ T ] with DaemonActor {
      @volatile private var v: Option[T] = None

      private case object Eval
      
      def isSet = !v.isEmpty

      def apply(): T = {
         if( v.isEmpty ) error( "Thread-based operations not supported" )
         v.get
      }

      def respond( k: T => Unit ) {
         v.map( k( _ )) getOrElse {
            val fut = this !! Eval
            fut.inputChannel.react {
               case _ => k( v.get )
            }
         }
      }

      def inputChannel: InputChannel[ T ] = channel

      override def finalize {
         println( "FutureActor finalized!" )
         super.finalize
      }

      def act() {
         val syncVar = new SyncVar[ T ]
         
         { fun( syncVar )} andThen {
            val syncVal = syncVar.get
            v = Some( syncVal )
            channel ! syncVal
            loop { react {
               case Eval => reply()
            }}
         }
      }
   }
}