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
 */
package de.sciss.tint.sc

import _root_.java.io.IOException
import _root_.java.net.SocketAddress
import _root_.java.util.{ ArrayList, HashMap, List, Map }

import _root_.de.sciss.scalaosc.{ OSCClient, OSCMessage }

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
 *  @version	0.34, 26-Nov-09
 */
class OSCMultiResponder( server: Server ) extends Object /* with OSCListener */ {

  private val allNodes			= new ArrayList[ OSCResponderNode ]()
  private val mapCmdToNodes		= new HashMap[ String, List[ OSCResponderNode ]]()

//	private static final boolean		debug				= false; 
	
  private var	resps				= new Array[OSCResponderNode]( 2 )
  private val	sync				= new Object

  // ---- constructor ----
//  c.addOSCListener( this )
  server.c.action_=( messageReceived )

  def getSync = sync

  def addNode( node: OSCResponderNode ) {
//  synchronized( sync ) {
      allNodes.add( node );
      var specialNodes = mapCmdToNodes.get( node.getCommandName )
      if( specialNodes == null ) {
        specialNodes = new ArrayList[ OSCResponderNode ]( 4 )
        mapCmdToNodes.put( node.getCommandName, specialNodes )
      }
      specialNodes.add( node )
//	}
  }

  def removeNode( node: OSCResponderNode ) {
//  synchronized( sync ) {
      val specialNodes = mapCmdToNodes.get( node.getCommandName )
      if( specialNodes != null ) {
        specialNodes.remove( node )
        allNodes.remove( node )
        if( allNodes.isEmpty ) {
          mapCmdToNodes.clear
        }
      }
//	}
  }
	
  protected def dispose {
//  synchronized( sync ) {
//      c.removeOSCListener( this )
      server.c.action_=( ignoreMessage )
      allNodes.clear
      mapCmdToNodes.clear
//    if( debug ) System.err.println( "OSCMultiResponder( client = " + c +"; hash = " + hashCode() + " ): dispose" );			
	  server.c.dispose
//  }
  }

// ------------ OSCListener interface ------------

  private def ignoreMessage( msg: OSCMessage, sender: SocketAddress, time: Long ) {}

  private def messageReceived( msg: OSCMessage, sender: SocketAddress, time: Long ) {
//  synchronized( sync ) {
	  // new stylee
	  msg match {
	 	  case nodeMsg: OSCNodeChange => server.nodeMgr.nodeChange( nodeMsg )
	 	  case _ =>
	  }
	  
	  // old stylee
      val cmdName = if( msg.name.charAt( 0 ) == 47 ) msg.name else "/" + msg.name
	  val specialNodes = mapCmdToNodes.get( cmdName )
      if( specialNodes == null ) return
      val numResps = specialNodes.size
      resps = specialNodes.toArray( resps )
//	}

    var i = 0
    while( i < numResps ) {
      try {
        resps( i ).messageReceived( msg, sender, time )
      } catch { case e: Exception => Server.printError( "messageReceived", e )}
      resps( i ) = null  // remove references, so gc can work
      i = i + 1
    }
  }
}