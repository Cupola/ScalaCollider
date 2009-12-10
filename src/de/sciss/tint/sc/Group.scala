/*
 *  Group.scala
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

import _root_.de.sciss.scalaosc.OSCMessage

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.11, 24-Nov-09
 */
object Group {
    def spawn: Group = {
		head( new Group( Server.default, 0 ))
	}

	def after( target: => Node ): Group = {
	  	val group = new Group( target.server );
	  	group.server.sendMsg( group.newMsg( target, 'addAfter ));
	  	group
	}
 
	def before( target: => Node ): Group = {
	  	val group = new Group( target.server )
	  	group.server.sendMsg( group.newMsg( target, 'addBefore ))
        group
	}
 
	def head( target: => Node ): Group = {
		val group = new Group( target.server )
		group.server.sendMsg( group.newMsg( target, 'addToHead ))
        group
	}

	def tail( target: => Node ): Group = {
	  	val group = new Group( target.server )
	  	group.server.sendMsg( group.newMsg( target, 'addToTail ))
        group
	}
 
	def replace( target: => Node ): Group = {
	  	val group = new Group( target.server )
	  	group.server.sendMsg( group.newMsg( target, 'addReplace ))
        group
	}
}

class Group( override val server: Server, override val id: Int )
extends Node( server, id ) {
	def this( server: Server ) = {
		this( server, server.nodes.nextID )
	}
  
	def this() = {
		this( Server.default )
	}

	def newMsg( target: => Node, addAction: Symbol ) : OSCMessage = {
		OSCMessage( "/g_new", id, Nodes.actionNumberFor( addAction ), target )
	}

	def dumpTree() : Unit = dumpTree( false )
  
	def dumpTree( postControls: Boolean ) {
		server.sendMsg( "/g_dumpTree", id, if( postControls ) 1 else 0 )
	}
  
	def freeAllMsg = OSCMessage( "/g_freeAll", id )
  
	def moveNodeToHeadMsg( node: Node ) : OSCMessage = {
		node.group = this
		OSCMessage( "/g_head", id, node.id )
	}
  
  	def moveNodeToTailMsg( node: Node ) : OSCMessage = {
		node.group = this
		OSCMessage( "/g_tail", id, node.id )
  	}

	override def toString = "Group( " + id + " )"
}