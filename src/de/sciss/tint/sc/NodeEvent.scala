/*
 *  NodeEvent.java
 *  Tintantmare
 *
 *  Copyright (c) 2004-2009 Hanns Holger Rutz. All rights reserved.
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
 *	contact@sciss.de , or visit http://www.sciss.de/jcollider
 *
 *
 *	JCollider is closely modelled after SuperCollider Language,
 *	often exhibiting a direct translation from Smalltalk to Java.
 *	SCLang is a software originally developed by James McCartney,
 *	which has become an Open Source project.
 *	See http://www.audiosynth.com/ for details.
 *
 *
 *  Changelog:
 *		02-Oct-05	created
 *		17-Sep-06	fixed bug in incorporate (now always returns false for simplicity)
 */
package de.sciss.tint.sc

import _root_.de.sciss.scalaosc.OSCMessage

/**
 *	These kind of events get delivered by a 
 *	node watcher to inform listeners about
 *	node status changes.
 *
 *  @author		Hanns Holger Rutz
 *  @version	0.34, 24-Nov-09
 */
object NodeEvent {
// --- ID values ---
	/**
	 *  returned by getID() : the node was created
	 */
	val GO			= 0

	/**
	 *  returned by getID() : the node was destroyed
	 */
	val END			= 1

	/**
	 *  returned by getID() : the node was resumed
	 */
	val ON			= 2

	/**
	 *  returned by getID() : the node was paused
	 */
	val OFF			= 3

	/**
	 *  returned by getID() : the node has moved
	 */
	val MOVE		= 4

	/**
	 *  returned by getID() : the event was created by a /n_query command
	 */
	val INFO		= 5

	/**
	 *  returned by getType() : the node is a synth
	 */
	val SYNTH		= 0

	/**
	 *  returned by getType() : the node is a group
	 */
	val GROUP		= 1

	/**
	 *  returned by getType() : the node type is unknown
	 */
	val UNKNOWN		= -1

	val VALID_CMDS = List( "/n_go", "/n_end", "/n_on", "/n_off", "/n_move", "/n_info" )
 
	/**
	 *	Constructs a <code>NodeEvent</code> from a valid node
	 *	notification OSC message. The provided node object is simply
	 *	stored for future reference through <code>getNode</code> and must
	 *	be updated by the caller according to the returned event.
	 *
	 *	@param	msg							OSC message such as <code>/n_go</code>
	 *	@param	source						who shall be known as the source of the generated event
	 *	@param	when						what is proposed time of the event generation
	 *	@param	node						a client side representation node to use for the event,
	 *										or <code>null</code> if no representation is known. The caller is
	 *										responsible for updating the node's status from the returned
	 *										event.
	 *
	 *	@throws	IllegalArgumentException	if the message doesn't contain a valid node message; you
	 *										can use <code>getIDFromOSCMessage</code> to determine if the
	 *										message is valid.
	 */
	def fromOSCMessage( msg: OSCMessage, source: Object, when: Long, node: Node ) : NodeEvent = {
		val eventID	= VALID_CMDS.indexOf( msg.name )
		if( eventID == -1 ) throw new IllegalArgumentException( "Not a valid node notification message : " + msg.name )
		
		val nodeID		= msg( 0 ).asInstanceOf[ Number ].intValue
		val parentID	= msg( 1 ).asInstanceOf[ Number ].intValue
		val predID		= msg( 2 ).asInstanceOf[ Number ].intValue
		val succID		= msg( 3 ).asInstanceOf[ Number ].intValue
		val nodeType	= msg( 4 ).asInstanceOf[ Number ].intValue
		val headID		= if( nodeType == GROUP ) msg( 5 ).asInstanceOf[ Number ].intValue else -1
		val tailID		= if( nodeType == GROUP ) msg( 6 ).asInstanceOf[ Number ].intValue else -1
		
		new NodeEvent( source, eventID, when, node, nodeID, parentID, predID, succID, nodeType, headID, tailID )
	}

	def fromOSCMessage( msg: OSCMessage, source: Object, when: Long ) : NodeEvent = {
		fromOSCMessage( msg, source, when, null )
	}
	
	/**
	 *	Queries the event ID which would be used if the event was
	 *	generated from a provided OSC message.
	 *	
	 *	@param	msg	the message to parse
	 *	@return	the corresponding event ID or <code>-1</code> if the message command
	 *			is not in the list of valid notification commands
	 */
	def getIDFromOSCMessage( msg: OSCMessage ) = VALID_CMDS.indexOf( msg.name )
 }

class NodeEvent( source: Object, id: Int, when: Long, node: Node, nodeID: Int,
                 parentID: Int, predID: Int, succID: Int, nodeType: Int,
                 headID: Int, tailID: Int )
extends java.awt.AWTEvent( source, id )
{
//	private final int	oldParentID;
//	private final int	oldPredID;
//	private final int	oldSuccID;

	// ---- constructor ----
//	if( node == null ) {
//		this.oldParentID	= -1;
//		this.oldPredID		= -1;
//		this.oldSuccID		= -1;
//	} else {
//		this.oldParentID	= node.getGroup()    == null ? -1 : node.getGroup().getNodeID();
//		this.oldPredID		= node.getPredNode() == null ? -1 : node.getPredNode().getNodeID();
//		this.oldSuccID		= node.getSuccNode() == null ? -1 : node.getSuccNode().getNodeID();
//	}
	

	/**
	 *	@return	the representation of the node whose status changed. this may return <code>null</code>
	 *			if the client side object is not known. in this case, use <code>getNodeID</code>
	 *			to query the node's identifier.
	 */
	def getNode = node

	/**
	 *	@return	the ID of the node whose status changed
	 */
	def getNodeID = nodeID

	/**
	 *	@return	the ID of the group in which this node sits. Note that if <code>getNode</code>
	 *			returns a valid node, you can also use <code>getParentGroup</code> on the
	 *			returned node.
	 */
	def getParentGroupID = parentID

	/**
	 *	@return	the ID of the group in which this node was sitting before the modification
	 *			occurred, or <code>-1</code>, if the node was not placed in a group.
	 */
//	def getOldParentGroupID = oldParentID

	/**
	 *	@return	the ID of the node sitting just before the modified node in the graph,
	 *			or <code>-1</code> if there is no predecessor. Note that if <code>getNode</code>
	 *			returns a valid node, you can also use <code>getPredNode</code> on the
	 *			returned node.
	 */
	def getPredNodeID = predID

	/**
	 *	@return	the ID of the node which was sitting just before the modified node before the
	 *			modification occured, or <code>-1</code> if there was no predecessor.
	 */
//	def getOldPredNodeID = oldPredID

	/**
	 *	@return	the ID of the node sitting just after the modified node in the graph,
	 *			or <code>-1</code> if there is no successor. Note that if <code>getNode</code>
	 *			returns a valid node, you can also use <code>getSuccNode</code> on the
	 *			returned node.
	 */
	def getSuccNodeID = succID
 
	/**
	 *	@return	the ID of the node which was sitting just after the modified node before the
	 *			modification occured, or <code>-1</code> if there was no successor.
	 */
//	def getOldSuccNodeID = oldSuccID

	/**
	 *	@return	the type node that was modified, one of <code>SYNTH</code> or <code>GROUP</code>.
	 *			other values might be returned if a new version of supercollider introduces
	 *			other node classes.
	 */
	def getNodeType = nodeType

	/**
	 *	@return	if the modified node is a group, returns the ID of the node being the group's
	 *			head element. otherwise (or when the group is empty) returns
	 *			<code>-1</code>. Note that if <code>getNode</code>
	 *			returns a valid group, you can also use <code>getHeadNode</code> on the
	 *			returned group.
	 */
	def getHeadNodeID = headID

	/**
	 *	@return	if the modified node is a group, returns the ID of the node being the group's
	 *			tail element. otherwise (or when the group is empty) returns
	 *			<code>-1</code>. Note that if <code>getNode</code>
	 *			returns a valid group, you can also use <code>getTailNode</code> on the
	 *			returned group.
	 */
	def getTailNodeID = tailID
	
	/**
	 *	Used by the <code>EventManager</code> to
	 *	fuse successive events together when they queue.
	 *	Do not call this method.
	 */
//	public boolean incorporate( BasicEvent oldEvent )
//	{
//		return false;
//	}
}