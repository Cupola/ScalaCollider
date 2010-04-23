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
import collection.immutable.Queue
import math._

/**
 * 	@version    0.14, 22-Apr-10
 */
object Server {
   private val allSync  = new AnyRef
   private var allVar   = Set.empty[ Server ]
   var default: Server  = null

   def all     = allVar

   private def add( s: Server ) {
      allSync.synchronized {
         allVar += s
         if( default == null ) default = s
      }
   }

   private def remove( s: Server ) {
      allSync.synchronized {
         allVar -= s
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

abstract class Server extends Model {
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
   val isLocal                                     = host.isLoopbackAddress || host.isSiteLocalAddress
   val rootNode                                    = new Group( this, 0 )
   val defaultGroup                                = new Group( this, 1 )
   val nodeMgr                                     = new NodeManager( this )
   val bufMgr                                      = new BufferManager( this )
   var latency                                     = 0.2f

   private val c                                   = {
      val client        = OSCClient( Symbol( options.protocol.value ), 0, host.isLoopbackAddress, ServerCodec )
      client.bufferSize = 0x10000
      client.target     = addr
      client.action     = multiResponder.messageReceived
      client
   }

   // ---- constructor ----
   {
      add( this )
      createNewAllocators
//    resetBufferAutoInfo
   }

   // ---- abstract methods ----
   protected def invokeOnMainThread( task: Runnable ) : Unit
   val name: String
   val options: ServerOptions
   val clientID: Int
   
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

      val whenBooted = (s: Server) => {
         try {
//            println( "notification is on" )
            s.register
            s.initTree
         }
         catch { case e: IOException => printError( "Server.boot", e )}
      }

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
            createNewAllocators
// XXX resetBufferAutoInfo

            addDoWhenBooted( whenBooted )
            if( bootThread.isEmpty ) {
               val thread	= new BootThread( createAliveThread )
               bootThread	= Some( thread )
               thread.start
            } else error( "Illegal state : boot thread still set" )
         }
         catch { case e => {
            removeDoWhenBooted( whenBooted )
            try {
               stopAliveThread
            }
            catch { case e => printError( "Server.boot", e )}
            condition = Offline // pendingCondition = NoPending
            throw e
         }}
      }
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
      c.start
   }

   protected[synth] def addResponderNode( resp: OSCResponderNode ) {
      multiResponder.addNode( resp )
   }

   protected[synth] def removeResponderNode( resp: OSCResponderNode ) {
      multiResponder.removeNode( resp )
   }

   protected[synth] def responderSync : AnyRef = multiResponder.nodeSync

   def dispose {
      condSync.synchronized {
         remove( this )
         c.action = (msg: OSCMessage, sender: SocketAddress, time: Long) => ()
         multiResponder.dispose
         c.dispose
      }
   }

   override def toString = "Server(" + name + ")"

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

//      // ---- constructor ----
//      timer.setInitialDelay( delayMillis )

      def start {
         stop
         timer = {
            val t = new Timer( "StatusWatcher", true )
            t.schedule( new TimerTask {
               def run = invokeOnMainThread( watcher )
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
         if( (pendingCondition == Booting) && (options.protocol.value == 'tcp) && !isConnected ) {
            try {
               startClient
            }
            catch { case e: IOException => printError( "Server.status", e )}
         } else {
            alive -= 1
            if( alive < 0 ) {
               condition = Offline
            }
            try {
               queryCounts
            }
            catch { case e: IOException => printError( "Server.status", e )}
         }
      }

      def statusReply( msg: OSCStatusReplyMessage ) {
         alive = deathBounces
         // note: put the counts before running
         // because that way e.g. the sampleRate
         // is instantly available
         counts     = msg
         condition  = Running
      }
   }

   object multiResponder extends Runnable {
      private val emptySet       = Set.empty[ OSCResponderNode ]
      private var mapCmdToNodes  = Map.empty[ String, Set[ OSCResponderNode ]]
      val nodeSync   		      = new AnyRef
      val msgSync		            = new AnyRef
      private var msgQueue: Queue[ ReceivedMessage ] = Queue.empty
      private var msgInvoked     = false

      def addNode( node: OSCResponderNode ) {
         nodeSync.synchronized {
            mapCmdToNodes += node.name -> (mapCmdToNodes.getOrElse( node.name, emptySet ) + node)
         }
      }

      def removeNode( node: OSCResponderNode ) {
         nodeSync.synchronized {
            mapCmdToNodes.get( node.name ).foreach( set => {
               val setNew = set - node
               if( setNew.isEmpty ) {
                  mapCmdToNodes -= node.name
               } else {
                  mapCmdToNodes += node.name -> setNew
               }
            })
         }
      }

      def dispose {
         nodeSync.synchronized { mapCmdToNodes = Map.empty }
      }

      // ------------ OSCListener interface ------------

      def messageReceived( msg: OSCMessage, sender: SocketAddress, time: Long ) {
         msgSync.synchronized {
            msgQueue = msgQueue.enqueue( ReceivedMessage( msg, sender, time ))
            if( !msgInvoked ) {
               msgInvoked = true
               invokeOnMainThread( this )
            }
         }
      }

      def run {
         val toProcess = msgSync.synchronized {
            msgInvoked  = false
            val result  = msgQueue
            msgQueue    = Queue.empty
            result
         }
         toProcess.foreach( rm => dispatchMessage( rm.msg, rm.sender, rm.time ))
      }

      private def dispatchMessage( msg: OSCMessage, sender: SocketAddress, time: Long ) {
         msg match {
            case nodeMsg:         OSCNodeChange           => nodeMgr.nodeChange( nodeMsg )
            case statusReplyMsg:  OSCStatusReplyMessage   => aliveThread.foreach( _.statusReply( statusReplyMsg ))
            case bufInfoMsg:      OSCBufferInfoMessage    => bufMgr.bufferInfo( bufInfoMsg )
            case _ =>
         }

         // old stylee
         val cmdName = if( msg.name.charAt( 0 ) == 47 ) msg.name else "/" + msg.name
         val nodes = nodeSync.synchronized { mapCmdToNodes.get( cmdName )}
         nodes.foreach( _.foreach( resp => {
            try {
               resp.messageReceived( msg, sender, time )
            } catch { case e: Exception => printError( "messageReceived", e )}
         }))
      }
   }

   // -------- internal class ReceivedMessage --------

   private case class ReceivedMessage( msg: OSCMessage, sender: SocketAddress, time: Long )
}

//object UniqueID { // XXX needs sync
//   private var id = 1000
//   def next : Int = { var result = id; id += 1; result }
//}