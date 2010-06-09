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

import de.sciss.scalaosc.{ OSCChannel, OSCClient, OSCMessage, OSCPacket, OSCTransport, TCP, UDP }
import java.net.{ ConnectException, DatagramSocket, InetAddress, InetSocketAddress, ServerSocket, SocketAddress }
import java.io.{ BufferedReader, File, InputStreamReader, IOException }
import java.util.{ Timer, TimerTask }
import actors.{ Actor, Channel, DaemonActor, Future, InputChannel, OutputChannel, TIMEOUT }
import collection.breakOut
import collection.immutable.Queue
import concurrent.SyncVar
import osc._
import math._

/**
 * 	@version    0.16, 09-Jun-10
 */
object Server {
   private val allSync  = new AnyRef
//   private var allVar   = Set.empty[ Server ]
   var default: Server  = null

//   def all     = allVar

   def defaultCmdPath = new File( System.getenv( "SC_HOME" ), "scsynth" ).getAbsolutePath

   @throws( classOf[ IOException ])
   def boot: BootingServer = boot()

   @throws( classOf[ IOException ])
   def boot( name: String = "localhost", options: ServerOptions = (new ServerOptionsBuilder).build,
             clientOptions: ClientOptions = (new ClientOptionsBuilder).build ) : BootingServer = {

      val addr = new InetSocketAddress( options.host, options.port )
      val c    = createClient( options.transport, addr )
      new BootingImpl( name, c, addr, options, clientOptions, true )
   }

   def allocPort( transport: OSCTransport ) : Int = {
      transport match {
         case TCP => {
            val ss = new ServerSocket( 0 )
            try {
               ss.getLocalPort()
            } finally {
               ss.close()
            }
         }
         case UDP => {
            val ds = new DatagramSocket()
            try {
               ds.getLocalPort()
            } finally {
               ds.close()
            }
         }
         case x => error( "Unsupported transport : " + x.name )
      }
   }

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
//   case object Booting extends Condition
   case object Offline extends Condition
   private case object Terminating extends Condition
   private case object NoPending extends Condition

   case class Counts( c: OSCStatusReplyMessage )

   private def createClient( transport: OSCTransport, addr: InetSocketAddress ) : OSCClient = {
      val client        = OSCClient( transport, 0, addr.getAddress.isLoopbackAddress, ServerCodec )
      client.bufferSize = 0x10000
      client.target     = addr
//      client.action     = OSCReceiverActor.messageReceived
      client
   }

   // -------- internal class BootThread --------

   object BootingImpl {
      case object Start
      case object Abort
      case object QueryServer
//      case object Aborted
   }
   
   private class BootingImpl @throws( classOf[ IOException ])
      ( val name: String, c: OSCClient, val addr: InetSocketAddress, val options: ServerOptions,
        clientOptions: ClientOptions, createAliveThread: Boolean )
   extends BootingServer {
      import BootingImpl._

      val p = {
         val processArgs   = options.toRealtimeArgs
         val directory     = new File( options.programPath ).getParentFile
         val pb            = new ProcessBuilder( processArgs: _* )
            .directory( directory )
            .redirectErrorStream( true )
         pb.start    // throws IOException if command not found or not executable
      }
      val inReader   = new BufferedReader( new InputStreamReader( p.getInputStream ))
      val postActor  = new DaemonActor {
         def act {
            var isOpen = true
            loopWhile( isOpen ) {
               val line = inReader.readLine
               isOpen = line != null
               if( isOpen ) println( line )
            }
         }
      }

      val processThread = new Thread {
         override def run = try {
            p.waitFor()
         } catch { case e: InterruptedException =>
            p.destroy()
         } finally {
            println( "scsynth terminated (" + p.exitValue +")" )
            bootActor ! BootingServer.Aborted
         }            
      }

      val bootActor: DaemonActor = new DaemonActor {
         def act { react { case Start => {
            dispatch( BootingServer.Booting )
            loop {
               if( processThread.isAlive ) {
                  try {
                     c.start
   //                  c.dumpOSC(1)
                     c.action = (msg, addr, when) => this ! msg
                     loop {
                        c.send( OSCServerNotifyMessage( true ))
                        reactWithin( 5000 ) {
                           case TIMEOUT => // loop is retried
                           case Abort => abortHandler( None )
                           case OSCMessage( "/done", "/notify" ) => loop {
                              c.send( OSCStatusMessage )
                              reactWithin( 500 ) {
                                 case TIMEOUT => // loop is retried
                                 case Abort => abortHandler( None )
                                 case counts: OSCStatusReplyMessage => {
                                    val s = new Server( name, c, addr, options, clientOptions )
                                    s.counts = counts
                                    dispatch( BootingServer.Running( s ))
                                    s.initTree
                                    // note that we optimistically assume that if we boot the server, it
                                    // will not die (exhausting deathBounces). if it crashes, the boot
                                    // thread's process will know anyway. this way we avoid stupid
                                    // server offline notifications when using slow asynchronous commands
                                    if( createAliveThread ) s.startAliveThread( 1.0f, 0.25f, Int.MaxValue )
                                    loop { react {
                                       case QueryServer => reply( s )
                                       case Abort => abortHandler( Some( s ))
                                       case BootingServer.Aborted => {
                                          s.bootThreadTerminated
                                          dispatch( BootingServer.Aborted )
                                          loop { react {
                                             case Abort => reply ()
                                             case QueryServer => reply( s )
   //                                          case _ =>
                                          }}
                                       }
   //                                    case _ =>
                                    }}
                                 }
                              }
                           }
                        }
                     }
                  }
                  catch { case e: ConnectException => { // thrown when in TCP mode and socket not yet available
                     reactWithin( 500 ) {
                        case Abort => abortHandler( None )
                     }
                  }}
               } else loop { react {
                  case Abort => reply ()
   //               case _ =>
               }}
            }
         }}}

         private def abortHandler( server: Option[ Server ]) {
             processThread.interrupt()
             val from = sender
             loop { react {
                case BootingServer.Aborted => {
                   server.foreach( _.bootThreadTerminated )
                   dispatch( BootingServer.Aborted )
                   from ! ()
                }
                case _ =>
             }}
          }
      }

      // ...and go
      postActor.start
      processThread.start
      bootActor.start

      def start { bootActor ! Start }
      lazy val server : Future[ Server ] = bootActor !! (QueryServer, { case s: Server => s })
      lazy val abort : Future[ Unit ] = bootActor !! (Abort, { case _ => ()})
//      def isStopped : Boolean

      override def toString = "<" + name + ">"
   }
}

trait ServerLike extends Model {
   def name: String
   def options: ServerOptions
   def addr: InetSocketAddress
}

object BootingServer {
   case object Booting
   case class Running( server: Server )
   case object Aborted
}
trait BootingServer extends ServerLike {
   def start : Unit
   def server : Future[ Server ]
   def abort : Future[ Unit ]
}

//abstract class Server extends Model {}
class Server private( val name: String, c: OSCClient, val addr: InetSocketAddress, val options: ServerOptions,
                      val clientOptions: ClientOptions )
extends ServerLike {
   server =>

   import Server._

   private var aliveThread: Option[StatusWatcher]	= None
//   private var bootThread: Option[BootThread]		= None
   private var countsVar							      = new OSCStatusReplyMessage( 0, 0, 0, 0, 0f, 0f, 0.0, 0.0 )
//   private var collBootCompletion					   = Queue.empty[ (Server) => Unit ]
   private val condSync                            = new AnyRef
   private var conditionVar: Condition 			   = Running // Offline
   private var pendingCondition: Condition      	= NoPending
//   private var bufferAllocatorVar: ContiguousBlockAllocator = null
//   private val host                                = InetAddress.getByName( options.host.value )

//   val addr                                        = new InetSocketAddress( host, options.port.value )
   val rootNode                                    = new Group( this, 0 )
   val defaultGroup                                = new Group( this, 1 )
   val nodeMgr                                     = new NodeManager( this )
   val bufMgr                                      = new BufferManager( this )
//   var latency                                     = 0.2f

   // ---- constructor ----
   {
      OSCReceiverActor.start
      c.action = OSCReceiverActor.messageReceived
      add( server )
   }

   def isLocal : Boolean = {
      val host = addr.getAddress
      host.isLoopbackAddress || host.isSiteLocalAddress
   }
   
   def isConnected = c.isConnected
   def isRunning = condSync.synchronized { conditionVar == Running }
//   def isBooting = condSync.synchronized { conditionVar == Booting }
   def isOffline = condSync.synchronized { conditionVar == Offline }
//   def bufferAllocator = bufferAllocatorVar

   object nodes {
      private var allocator = new NodeIDAllocator( clientOptions.clientID, clientOptions.nodeIDOffset )

      def nextID = allocator.alloc
   }
  
   object busses {
      private var controlAllocator = new ContiguousBlockAllocator( options.controlBusChannels )
      private var audioAllocator = new ContiguousBlockAllocator( options.audioBusChannels, options.firstPrivateBus )

      def allocControl( numChannels: Int ) = controlAllocator.alloc( numChannels )
      def allocAudio( numChannels: Int ) = audioAllocator.alloc( numChannels )
      def freeControl( index: Int ) = controlAllocator.free( index )
      def freeAudio( index: Int ) = audioAllocator.free( index )
   }

   object buffers {
      private var allocator = new ContiguousBlockAllocator( options.audioBuffers )

      def alloc( numChannels: Int ) = allocator.alloc( numChannels )
      def free( index: Int ) = allocator.free( index )
   }

   private object uniqueID {
      private var id = 0
      def nextID = this.synchronized { val res = id; id += 1; res }
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
            server ! p // only after addHandler!
            futCh.reactWithin( timeOut ) {
               case TIMEOUT   => OSCReceiverActor.removeHandler( oh )
               case r         =>
            }
         }
      }
      a.start()
// NOTE: race condition, addHandler might take longer than
// the /done, notify!
//      this ! p
      a
   }

   def counts = countsVar
   private[synth] def counts_=( newCounts: OSCStatusReplyMessage ) {
      countsVar = newCounts
      dispatch( Counts( newCounts ))
   }

   def sampleRate = counts.sampleRate
  
   def dumpTree : Unit = dumpTree( false )

   def dumpTree( controls: Boolean ) {
      rootNode.dumpTree( controls )
   }
  
   def condition = condSync.synchronized { conditionVar }
   private[synth] def condition_=( newCondition: Condition ) {
      condSync.synchronized {
         if( newCondition != conditionVar ) {
            conditionVar = newCondition
            if( newCondition == Offline ) {
               pendingCondition = NoPending
               serverLost
            }
//            else if( newCondition == Running ) {
//               if( pendingCondition == Booting ) {
//                  pendingCondition = NoPending
//                  collBootCompletion.foreach( action => try {
//                        action.apply( this )
//                     }
//                     catch { case e => e.printStackTrace() }
//                  )
//                  collBootCompletion = Queue.empty
//               }
//            }
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

   def syncMsg : OSCSyncMessage = syncMsg()
   def syncMsg( id: Int = uniqueID.nextID ) = OSCSyncMessage( id )

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

   private def serverLost {
      nodeMgr.clear
      bufMgr.clear
      OSCReceiverActor.clear
   }

   private def bootThreadTerminated {
      condSync.synchronized {
//         bootThread = None
         stopAliveThread
         condition = Offline
      }
   }

   def quit {
      this ! quitMsg
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

   private[synth] def addResponder( resp: OSCResponder ) {
      OSCReceiverActor.addHandler( resp )
   }

   private[synth] def removeResponder( resp: OSCResponder ) {
      OSCReceiverActor.removeHandler( resp )
   }

   private[synth] def initTree {
      nodeMgr.register( defaultGroup )
      server ! defaultGroup.newMsg( rootNode, addToHead )
   }

   def dispose {
      condSync.synchronized {
         remove( this )
         c.dispose // = (msg: OSCMessage, sender: SocketAddress, time: Long) => ()
         OSCReceiverActor.dispose
//         c.dispose
      }
   }

   override def toString = "<" + name + ">"

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
//               serverContacted
               condition = Running
            }
         }
      }
   }

   private object OSCReceiverActor extends DaemonActor {
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
}