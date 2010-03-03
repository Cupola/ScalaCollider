/*
 *  OSCMultiResponder.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2009 Hanns Holger Rutz. All rights reserved.
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
 *    28-Jan-10   dispatching messages from the AWT event thread,
 *                at least at the moment where kontur needs to get
 *                some more determinism. Later we ought to use
 *                actors or so
 */
package de.sciss.tint.sc

import java.awt.EventQueue
import java.io.IOException
import java.net.SocketAddress
//import java.util.{ ArrayList, HashMap, List, Map }
import scala.collection.immutable.{ Queue }

import de.sciss.scalaosc.{ OSCClient, OSCMessage }

/**
 *	Despite the name, the <code>OSCMultiResponder</code>
 *	mimics the SClang counter part only superficially.
 *	It absorbs the whole <code>OSCResponder</code> class
 *	and is based on the <code>NetUtil</code> OSC library.
 *	<p>
 *	While the super class <code>OSCReceiver</code> allows
 *	only a coarse message filtering, using the simple
 *	<code>OSCListener</code> interface, the <code>OSCMultiResponder</code>
 *	maintains a map of OSC command names and listeners
 *	(<code>OSCResponderNode</code>s) who wish to be
 *	informed about only this particular type of messages.
 *	<p>
 *	When a new node is added using the <code>addNode</code>
 *	method, the static list of all multi responders is searched
 *	for the given server address. If it exists, the corresponding
 *	multi responder is used, otherwise a new multi responder is
 *	created. Likewise, when <code>removeNode</code> is called,
 *	the multi responder checks if all nodes have been removed,
 *	and if so will terminate the OSC receiver.
 *	<p>
 *	To keep the responder permanently active, the server creates
 *	a multi responder for its address upon instantiation.
 *
 *  @author		Hanns Holger Rutz
 *  @version	0.35, 03-Mar-10
 */
object OSCMultiResponder {
   private val emptySet = Set.empty[ OSCResponderNode ]
   private def ignoreMessage( msg: OSCMessage, sender: SocketAddress, time: Long ) {}
}

class OSCMultiResponder( server: Server ) extends Runnable /* with OSCListener */ {
   import OSCMultiResponder._

//  private val allNodes			= new ArrayList[ OSCResponderNode ]()
  private var mapCmdToNodes   = Map.empty[ String, Set[ OSCResponderNode ]]

//	private static final boolean		debug				= false; 
	
//  private var	resps				= new Array[OSCResponderNode]( 2 )
  private val	sync				= new AnyRef

  // ---- constructor ----
//  c.addOSCListener( this )
//  server.c.action_=( messageReceived )
  server.c.action_=( messageReceived )

  def getSync = sync

  def addNode( node: OSCResponderNode ) {
//  synchronized( sync ) {
//      allNodes.add( node );
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
	
  protected def dispose {
//  synchronized( sync ) {
//      c.removeOSCListener( this )
      server.c.action_=( ignoreMessage )
//      allNodes.clear
      mapCmdToNodes = Map.empty
//    if( debug ) System.err.println( "OSCMultiResponder( client = " + c +"; hash = " + hashCode() + " ): dispose" );			
	  server.c.dispose
//  }
  }

// ------------ OSCListener interface ------------

   private var msgQueue: Queue[ ReceivedMessage ] = Queue.Empty
   private var msgInvoked = false

   private def messageReceived( msg: OSCMessage, sender: SocketAddress, time: Long ) {
      sync.synchronized {
         msgQueue = msgQueue.enqueue( ReceivedMessage( msg, sender, time ))
         if( !msgInvoked ) {
            msgInvoked = true
            EventQueue.invokeLater( this )
         }
      }
   }

   def run {
      val toProcess = sync.synchronized {
         msgInvoked = false
         val result = msgQueue
         msgQueue = Queue.Empty
         result
      }
      toProcess.foreach( rm => dispatchMessage( rm.msg, rm.sender, rm.time ))
   }

   private def dispatchMessage( msg: OSCMessage, sender: SocketAddress, time: Long ) {
      server.messageReceived( msg, sender, time )

	   // old stylee
      val cmdName = if( msg.name.charAt( 0 ) == 47 ) msg.name else "/" + msg.name
	   mapCmdToNodes.get( cmdName ).foreach( _.foreach( resp => {
         try {
            resp.messageReceived( msg, sender, time )
         } catch { case e: Exception => Server.printError( "messageReceived", e )}
      }))
   }

   private case class ReceivedMessage( msg: OSCMessage, sender: SocketAddress, time: Long )
}