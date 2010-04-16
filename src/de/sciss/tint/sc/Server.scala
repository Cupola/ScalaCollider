/*
 *  Server.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.tint.sc

import de.sciss.scalaosc.{ OSCBundle, OSCChannel, OSCClient, OSCMessage }
import java.net.{ ConnectException, InetAddress, InetSocketAddress, SocketAddress }
import java.io.{ BufferedInputStream, File, InputStream, IOException }
import java.util.{ Timer, TimerTask }
import scala.collection.immutable.{ Queue }
import scala.collection.mutable.{ ListBuffer }
import scala.math._

/**
 *    @author		Hanns Holger Rutz
 * 	@version    0.13, 08-Mar-10
 */
object Server {
   private var allVar = Set.empty[ Server ]

   var default: Server = null
   def all = allVar
   
   def printError( name: String, t: Throwable ) {
      println( name + " : " )
      t.printStackTrace
   }

   case object Running
   case object Booting
   case object Offline
   case class Counts( c: OSCStatusReplyMessage )
   private case object Terminating
   private case object NoPending
}

abstract class Server extends Model {
   import Server._

   private val syncBootThread						      = new AnyRef
   private var aliveThread: Option[StatusWatcher]	= None
   private var bootThread: Option[BootThread]		= None
   private var countsVar							      = new OSCStatusReplyMessage( 0, 0, 0, 0, 0f, 0f, 0.0, 0.0 )
   private val collBootCompletion					   = new ListBuffer[ (Server) => Unit ]()
   private var conditionVar: AnyRef 				   = Offline
   private var pendingCondition: AnyRef	      	= NoPending
   private var bufferAllocatorVar: ContiguousBlockAllocator = null
   private val host                                = InetAddress.getByName( options.host.value )

   val addr                                        = new InetSocketAddress( host, options.port.value )
   val isLocal                                     = host.isLoopbackAddress || host.isSiteLocalAddress
   val rootNode                                    = new Group( this, 0 )
   val defaultGroup                                = new Group( this, 1 )
   val nodeMgr                                     = new NodeManager( this )
   val bufMgr                                      = new BufferManager( this )
   var latency                                     = 0.2f

   private val c                                   = {
      val client        = OSCClient( options.protocol.value, 0, host.isLoopbackAddress, ServerCodec )
      client.bufferSize = 0x10000
      client.target     = addr
      client
   }

   private val multi                               = new OSCMultiResponder  // must be after 'c'
   
   // ---- constructor ----
   {
      allVar += this
      if( default == null ) default = this
      createNewAllocators
//    resetBufferAutoInfo
//    try { c.connect }
   }

   // ---- abstract methods ----
   protected def invokeOnMainThread( task: Runnable ) : Unit
   val name: String
   val options: ServerOptions
   val clientID: Int
   

   def isConnected = c.isConnected
   def isRunning = conditionVar == Running
   def isBooting = conditionVar == Booting
   def isOffline = conditionVar == Offline
//  def getBufferAllocator = bufferAllocator
   def bufferAllocator = bufferAllocatorVar

   object nodes {
      private var allocator : NodeIDAllocator = _
    
      reset
    
      def nextID = { allocator.alloc }
      def reset = { allocator = new NodeIDAllocator( clientID, options.initialNodeID )}
   }
  
   object busses {
      protected var controlAllocator : ContiguousBlockAllocator = _
      protected var audioAllocator : ContiguousBlockAllocator = _
    
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
  
   def sendMsg( cmd: String, p: Any* ) {
      c.send( OSCMessage( cmd, p: _* ))
   }
  
   def sendMsg( msg: OSCMessage ) {
      c.send( msg )
   }

	def sendBundle( delta: Double, msgs: OSCMessage* ) {
      // XXX eventually use logical clock
	   c.send(
	      if( delta >= 0 ) {
	    	   val absSecs			= System.currentTimeMillis * 0.001 + delta
	    	   val secsSince1900	= absSecs.toLong + 2208988800L;
	    	   val secsFractional= ((absSecs % 1.0) * 0x100000000L).toLong
	    	   val raw				= (secsSince1900 << 32) | secsFractional;
	    	   OSCBundle( raw, msgs: _* )
	      } else {
	     	   OSCBundle( msgs: _* )
	      }
	   )
	}

   def sendBundle( bndl: OSCBundle ) {
      c.send( bndl )
   }

   def counts = countsVar
   protected[sc] def counts_=( newCounts: OSCStatusReplyMessage ) {
      countsVar = newCounts
      dispatch( Counts( newCounts ))
   }

   def sampleRate = counts.sampleRate
  
   def dumpTree : Unit = dumpTree( false )

   def dumpTree( controls: Boolean ) {
      rootNode.dumpTree( controls )
   }
  
   def condition = conditionVar
   protected[sc] def condition_=( newCondition: AnyRef ) {
      if( newCondition != conditionVar ) {
         conditionVar = newCondition
         if( newCondition == Offline ) {
//          if( pendingCondition == Terminating ) {
               pendingCondition = NoPending
//          }
         } else if( newCondition == Running ) {
            if( pendingCondition == Booting ) {
               pendingCondition = NoPending
               collBootCompletion.foreach( _.apply( this ))
               collBootCompletion.clear
            }
         }
         dispatch( newCondition )
      }
   }

//  protected[sc] def getMultiResponder = multi

   def startAliveThread( delay: Float = 0.25f, period: Float = 0.25f, deathBounces: Int = 25 ) {
//    synchronized( syncBootThread ) {
      if( aliveThread.isEmpty ) {
         val statusWatcher = new StatusWatcher( delay, period, deathBounces )
         aliveThread = Some( statusWatcher )
         statusWatcher.start
      }
//    }
   }

   def stopAliveThread {
//  synchronized( syncBootThread ) {
      aliveThread.foreach( _.stop )
      aliveThread = None
//  }
  }

   def queryCounts {
      sendMsg( OSCStatusMessage )
   }

   // note: this is called register instead of
   // notify to avoid conflict with notify in java.lang.Object
   def register( onOff: Boolean = true ) {
      sendMsg( notifyMsg( onOff ))
   }

   def notifyMsg( onOff: Boolean = true ) : OSCMessage =
      OSCMessage( "/notify", if( onOff ) 1 else 0 )

   def dumpOSC( mode: Int = OSCChannel.DUMP_TEXT ) {
      c.dumpIncomingOSC( mode, filter = _ match {
         case m: OSCStatusReplyMessage => false
         case _ => true
      })
      c.dumpOutgoingOSC( mode, filter = _ match {
// fucking match doesn not work, need case object here eventually!
//        case m: OSCStatusMessage.type => false
         case OSCMessage( "/status" ) => false
         case _ => true
      })
   }

   def boot: Unit = boot( true )
   def boot( createAliveThread: Boolean ) {
      if( pendingCondition != NoPending ) {
         println( "Server:boot - ongoing operations" )
         return
      }
      if( conditionVar == Running ) {
         println( "Server:boot - already running" )
         return
      }
    
      if( !isLocal ) throw new IllegalStateException( "Server.boot : only allowed for local servers!" )
      
      val whenBooted = (s: Server) => {
         try {
            println( "notification is on" )
            s.register()
            s.initTree	// XXX inefficient since it re-created the node allocator
         }
         catch { case e: IOException => printError( "Server.boot", e )}
      }

      pendingCondition = Booting
      condition = Booting

      try {
         createNewAllocators
// XXX resetBufferAutoInfo
			
         addDoWhenBooted( whenBooted )
         bootServerApp( createAliveThread )
      }
      catch { case e /*: IOException*/ => {
         removeDoWhenBooted( whenBooted )
         try {
            stopAliveThread
         }
         catch { case e /*: IOException*/ => printError( "Server.boot", e )}
//      setBooting( false )
         condition = Offline // pendingCondition = NoPending
         throw e
      }}
   }
  
   private def bootThreadTerminated {
      bootThread = None
      stopAliveThread
      condition = Offline
   }
  
   // note: we do _not_ reset the nodeIDallocator here
   def initTree {
//    println( "initTree" )
      sendMsg( defaultGroup.newMsg( rootNode, addToHead ))
//    sendMsg( OSCMessage( "/g_new", 1, 0, 0 ))
   }
  
   def addDoWhenBooted( action: (Server) => Unit ) {
      collBootCompletion += action
   }
  
   def removeDoWhenBooted( action: (Server) => Unit ) {
      collBootCompletion -= action
   }
 
   private def createNewAllocators {
      nodes.reset
      busses.reset
      bufferAllocatorVar = new ContiguousBlockAllocator( options.audioBuffers.value )
   }

   private def bootServerApp( createAliveThread: Boolean ) {
      if( bootThread.isEmpty ) {
//    println( "about to boot " + this.name + "; '" + this.options.program.value + "'" )
         var thread	= new BootThread( createAliveThread )
         bootThread	= Some( thread )
//       condition	= 'booting
         thread.start
      }
   }
  
   def quit {
      sendMsg( quitMsg )
      println( "/quit sent" )
      cleanUpAfterQuit
   }

   def quitMsg = OSCMessage( "/quit" )

   private def cleanUpAfterQuit {
      try {
         stopAliveThread
         pendingCondition = Terminating
      }
      catch { case e: IOException => printError( "Server.cleanUpAfterQuit", e )}
   }
  
   def startClient {
      c.start
   }

   protected[sc] def addResponderNode( resp: OSCResponderNode ) {
      multi.addNode( resp )
   }

   protected[sc] def removeResponderNode( resp: OSCResponderNode ) {
      multi.removeNode( resp )
   }

   def dispose {
      multi.dispose
      c.dispose
   }

   override def toString = "Server( " + name + " )"

   // -------- internal class BootThread -------- 

   private class BootThread( createAliveThread: Boolean )
   extends Thread {
      var keepRunning = true

//  private val folder = new File( "/Users/rutz/Documents/devel/fromSVN/SuperCollider3/build" )
      private val program = options.programPath.value
      println( "Booting '" + program + "'" )
      private val file = new File( program )
      private val processArgs = options.toRealtimeArgs.toArray
      private val pb = new ProcessBuilder( processArgs: _* )
         .directory( file.getParentFile )
         .redirectErrorStream( true )

      override def run {
         var cStarted = false
         var pRunning = true
         val inBuf	= new Array[Byte](128)
         try {
            val p			= pb.start
            val inStream	= new BufferedInputStream( p.getInputStream )
            while( keepRunning && pRunning ) {
               if( !cStarted ) {
                  try {
                     startClient
                     cStarted = true
                     // note that we optimistically assume that if we boot the server, it
                     // will not die (exhausting deathBounces). if it crashes, the boot
                     // thread's process will know anyway. this way we avoid stupid
                     // server offline notifications when using slow asynchronous commands
                     if( createAliveThread ) startAliveThread( 1.0f, 0.25f, Int.MaxValue )
                  }
                  catch { case e: ConnectException => } // thrown when in TCP mode and socket not yet available
               }
               try {
                  Thread.sleep( 500 )   // a kind of cheesy way to wait for the program to end
               }
               catch { case e: InterruptedException => }

               handleConsole( inStream, inBuf )
//             handleConsole( errStream, errBuf )
               try {
                  val resultCode	= p.exitValue
                  pRunning			= false
//                p					= null
                  println( "scsynth terminated (" + resultCode +")" )
               }
               catch { case e: IllegalThreadStateException => } // gets thrown if we call exitValue() while sc still running
            } // while( keepScRunning && pRunning )
         }
         catch { case e: IOException => printError( "BootThread.run", e )}
         finally {
            bootThreadTerminated  // ! must be before setRunning !
         }
      }

      // redirect console
      private def handleConsole( stream: InputStream, buf: Array[Byte] ) : Unit = {
         try {
            while( stream.available > 0 ) {
               val i = min( buf.length, stream.available )
               stream.read( buf, 0, i )
//             printStream.write( buf, 0, i )
               print( new String( buf, 0, i ))
            }
         }
         catch { case e: IOException => } // ignored XXX
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

   private class OSCMultiResponder extends Runnable {
      private val emptySet       = Set.empty[ OSCResponderNode ]
      private var mapCmdToNodes  = Map.empty[ String, Set[ OSCResponderNode ]]
      private val	sync				= new AnyRef
      private var msgQueue: Queue[ ReceivedMessage ] = Queue.empty
      private var msgInvoked     = false

      // ---- constructor ----
      {
         c.action = messageReceived
      }

      def addNode( node: OSCResponderNode ) {
   //  synchronized( sync ) {
         mapCmdToNodes += node.name -> (mapCmdToNodes.getOrElse( node.name, emptySet ) + node)
   //	}
      }

      def removeNode( node: OSCResponderNode ) {
   //  synchronized( sync ) {
         mapCmdToNodes.get( node.name ).foreach( set => {
            val setNew = set - node
            if( setNew.isEmpty ) {
               mapCmdToNodes -= node.name
            } else {
               mapCmdToNodes += node.name -> setNew
            }
         })
   //	}
      }

      def dispose {
   //  synchronized( sync ) {
         c.action = (msg: OSCMessage, sender: SocketAddress, time: Long) => ()
         mapCmdToNodes = Map.empty
   //  }
      }

      // ------------ OSCListener interface ------------

      private def messageReceived( msg: OSCMessage, sender: SocketAddress, time: Long ) {
         sync.synchronized {
            msgQueue = msgQueue.enqueue( ReceivedMessage( msg, sender, time ))
            if( !msgInvoked ) {
               msgInvoked = true
               invokeOnMainThread( this )
//               EventQueue.invokeLater( this )
            }
         }
      }

      def run {
         val toProcess = sync.synchronized {
            msgInvoked = false
            val result = msgQueue
            msgQueue = Queue.empty
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
         mapCmdToNodes.get( cmdName ).foreach( _.foreach( resp => {
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