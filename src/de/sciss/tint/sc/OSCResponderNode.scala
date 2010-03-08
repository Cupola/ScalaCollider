/*
 *  OSCResponderNode.scala
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

import java.io.IOException
import java.net.SocketAddress
import de.sciss.scalaosc.OSCMessage

/**
 *	Similar operation as the SClang class
 *	of the same name, but slightly different implementation,
 *	based on the new <code>OSCMultiResponder</code> class
 *	and <code>NetUtil</code>.
 *	<P>
 *	<B>As of v0.29</B> the creator signature had to be changed to use a <code>Server</code> instead
 *	of a network address, unfortunately (a side effect of using <code>OSCClient</code> in <code>Server</code>
 *	and <code>OSCMultiResponder</code>). So you may need to update old code.
 *
 *  @author		Hanns Holger Rutz
 *  @version	0.12, 08-Mar-10
 */
class OSCResponderNode( server: Server, val name: String, action: (OSCMessage, SocketAddress, Long) => Unit ) {
  
// private val		multi			= server.getMultiResponder
// private val		sync			= multi.getSync

	private var		removeWhenDoneVal	= false
	private var		listening			= false		

	/**
	 *	Queries the name which is
	 *	used as the message filter
	 *
	 *	@return	the name of the OSC command to which this
	 *			responder listens
	 */
//	def getCommandName = cmdName

	/**
	 *	Adds the node to the list of actively listening nodes.
	 *	If you are uncertain about the node's state, check
	 *	<code>isListening</code> first, since this method will
	 *	throw an <code>IllegalStateException</code> if you try
	 *	to add it twice.
	 *
	 *	@return		the responder node (for convenience)
	 *
	 *	@see		#remove()
	 *	@see		#isListening()
	 *
	 *	@throws		IllegalStateException	if the node has already been added
	 */
	def add : OSCResponderNode = {
//	  synchronized( sync ) {
	    if( !listening ) {
           server.addResponderNode( this )
           listening = true
       }
       this
//	  }
	}
	
	/**
	 *	Queries the node's state.
	 *
	 *	@return		<code>true</code> if the node is active
	 *				(was added), <code>false</code> otherwise
	 *				(newly created node or removed)
	 */
	def isListening = listening

	/**
	 *	Tags the node to remove itself after the next
	 *	unfiltered message arrived. If the node shall
	 *	receive exactly one message, a clean code must
	 *	call this method before calling the <code>add</code>
	 *	method.
	 *
	 *	@return		the responder node (for convenience)
	 */
	def removeWhenDone : OSCResponderNode = {
		removeWhenDoneVal = true
		this
	}
	
	/**
	 *	This method is called as part of
	 *	the implementation of the <code>OSCListener</code>
	 *	interface. It dispatches the message to
	 *	the action. If <code>removeWhenDone</code>
	 *	was called, it will remove the node after
	 *	the action returns.
	 *
	 *	@see	#removeWhenDone()
	 */
	def messageReceived( msg: OSCMessage, sender: SocketAddress, time: Long ) {
		if( listening ) {
			try {
				action( msg, sender, time )
			} catch { case e: Exception => Server.printError( "messageReceived", e )}
			if( removeWhenDoneVal ) {
				try {
					remove	// OSCMultiResponder will take care of thread issues
				} catch { case e: IOException => Server.printError( "remove", e )}
			}
		}
	}

	/**
	 *	Removes the node from the list of actively
	 *	listening nodes. If the node was already removed,
	 *	this method does nothing.
	 *
	 *	@return		the responder node (for convenience)
	 *
	 *	@see		#add()
	 */
	def remove : OSCResponderNode = {
//	  synchronized( sync ) {
        listening = false
        server.removeResponderNode( this )
        this
//    }
	}
}