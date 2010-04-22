/*
 *  NodeManager.scala
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

import collection.immutable.IntMap

/**
 *    @version 0.12, 22-Apr-10
 */
object NodeManager {
   trait NodeChange { def node: Node; def msg: OSCNodeChange }
   case class NodeGo(   node: Node, msg: OSCNodeChange ) extends NodeChange
   case class NodeEnd(  node: Node, msg: OSCNodeChange ) extends NodeChange
   case class NodeOn(   node: Node, msg: OSCNodeChange ) extends NodeChange
   case class NodeOff(  node: Node, msg: OSCNodeChange ) extends NodeChange
   case class NodeMove( node: Node, msg: OSCNodeChange ) extends NodeChange
}

class NodeManager( server: Server ) extends Model {

   import NodeManager._
    
	private var nodes    = IntMap.empty[ Node ]
	private var autoAdd  = true
   private val sync     = new AnyRef
	
	// ---- constructor ----
	{
		val rootNode = server.rootNode // new Group( server, 0 )
		nodes += rootNode.id -> rootNode
      if( server.isRunning ) {
         val defaultGroup = server.defaultGroup
         nodes += defaultGroup.id -> defaultGroup
      }
	}

	def nodeChange( e: OSCNodeChange ) {
		val node = nodes.get( e.nodeID ) getOrElse {
         if( autoAdd && (e.name == "/n_go") && nodes.contains( e.parentID )) {
            val created = e match {
               case ee: OSCSynthChange => new Synth( "?", server, e.nodeID ) // que se puede acer...
               case ee: OSCGroupChange => new Group( server, e.nodeID )
            }
            sync.synchronized {
               nodes += created.id -> created
            }
            created
         } else return
      }
		
		e.name match {
			case "/n_go"   => nodeGo( node, e )
			case "/n_end"  => nodeEnd( node, e )
			case "/n_off"  => nodeOff( node, e )
			case "/n_on"   => nodeOn( node, e )
			case "/n_move" => nodeMove( node, e )
			case _ =>
		}
	}

	private def nodeGo( node: Node, e: OSCNodeChange ) {
      dispatchBoth( NodeGo( node, e ))
	}
	
	private def nodeEnd( node: Node, e: OSCNodeChange ) {
		val parentO = nodes.get( e.parentID )
		
      sync.synchronized {
   		nodes -= node.id
      }
      dispatchBoth( NodeEnd( node, e ))
	}

	private def nodeOff( node: Node, e: OSCNodeChange ) {
      dispatchBoth( NodeOff( node, e ))
	}

	private def nodeOn( node: Node, e: OSCNodeChange ) {
      dispatchBoth( NodeOn( node, e ))
	}

	private def nodeMove( node: Node, e: OSCNodeChange ) {
      dispatchBoth( NodeMove( node, e ))
	}

   private def dispatchBoth( change: NodeChange ) {
      dispatch( change )
      change.node.updated( change )
   }
	
	// eventually this should be done automatically
	// by the message dispatch management
	def register( node: Node ) {
      sync.synchronized {
   		nodes += node.id -> node
      }
	}
	
	def unregister( node: Node ) {
      sync.synchronized {
   		nodes -= node.id
      }
	}
}