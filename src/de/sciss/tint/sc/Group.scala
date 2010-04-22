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

import de.sciss.scalaosc.OSCMessage

/**
 *    @version	0.12, 22-Apr-10
 */
object Group {
    def spawn: Group = {
		head( Server.default.defaultGroup )
	}

	def after( target: Node ): Group = {
	  	val group = new Group( target.server )
	  	group.server ! group.newMsg( target, addAfter )
	  	group
	}
 
	def before( target: Node ): Group = {
	  	val group = new Group( target.server )
	  	group.server ! group.newMsg( target, addBefore )
      group
	}
 
	def head( target: Group ): Group = {
		val group = new Group( target.server )
		group.server ! group.newMsg( target, addToHead )
      group
	}

	def tail( target: Group ): Group = {
	  	val group = new Group( target.server )
	  	group.server ! group.newMsg( target, addToTail )
      group
	}
 
	def replace( target: Node ): Group = {
	  	val group = new Group( target.server )
	  	group.server ! group.newMsg( target, addReplace )
      group
	}
}

class Group( server: Server, id: Int )
extends Node( server, id ) {
	def this( server: Server ) = this( server, server.nodes.nextID )
	def this() = this( Server.default )

	def newMsg( target: Node, addAction: AddAction ) =
		OSCMessage( "/g_new", id, addAction.id, target.id )

   def dumpTree: Unit = dumpTree( false )
	def dumpTree( postControls: Boolean ) {
		server ! dumpTreeMsg( postControls )
	}

   def dumpTreeMsg : OSCMessage = dumpTreeMsg( false )
   def dumpTreeMsg( postControls: Boolean ) = OSCMessage( "/g_dumpTree", id, if( postControls ) 1 else 0 )

   def freeAll { server ! freeAllMsg }
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