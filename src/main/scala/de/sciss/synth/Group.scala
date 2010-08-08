/*
 *  Group.scala
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

import osc.{ OSCGroupDeepFreeMessage, OSCGroupDumpTreeMessage, OSCGroupFreeAllMessage, OSCGroupHeadMessage,
             OSCGroupNewInfo, OSCGroupNewMessage, OSCGroupQueryTreeMessage, OSCGroupTailMessage }

/**
 *    @version	0.12, 04-Jun-10
 */
object Group {
    def play: Group = {
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

   def apply( server: Server ) : Group = apply( server, server.nodes.nextID )
   def apply() : Group = apply( Server.default )
}

case class Group( server: Server, id: Int )
extends Node {
	def this( server: Server ) = this( server, server.nodes.nextID )
	def this() = this( Server.default )

	def newMsg( target: Node, addAction: AddAction ) =
		OSCGroupNewMessage( OSCGroupNewInfo( id, addAction.id, target.id ))

   def dumpTree: Unit = dumpTree( false )
	def dumpTree( postControls: Boolean ) {
		server ! dumpTreeMsg( postControls )
	}

   def dumpTreeMsg : OSCGroupDumpTreeMessage = dumpTreeMsg( false )
   def dumpTreeMsg( postControls: Boolean ) = OSCGroupDumpTreeMessage( id -> postControls )

   def queryTreeMsg( postControls: Boolean ) = OSCGroupQueryTreeMessage( id -> postControls )

   def freeAll { server ! freeAllMsg }
	def freeAllMsg = OSCGroupFreeAllMessage( id )
  
   def deepFree { server ! deepFreeMsg }
	def deepFreeMsg = OSCGroupDeepFreeMessage( id )

	def moveNodeToHeadMsg( node: Node ) = OSCGroupHeadMessage( id -> node.id )
  	def moveNodeToTailMsg( node: Node ) = OSCGroupTailMessage( id -> node.id )
}