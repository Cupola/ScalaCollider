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
   abstract sealed class NodeChange { def node: Node; def msg: OSCNodeChange }
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

	def nodeChange( e: OSCNodeChange ) : Unit = e match {
      case OSCNodeGoMessage( nodeID, _ ) => {
         val node = nodes.get( nodeID ) getOrElse {
            if( autoAdd && nodes.contains( e.info.parentID )) {
               val created = e.info match {
                  case ee: OSCSynthInfo => new Synth( "?", server, nodeID ) // que se puede acer...
                  case ee: OSCGroupInfo => new Group( server, nodeID )
               }
               sync.synchronized {
                  nodes += nodeID -> created
               }
               created
            } else return
         }
         dispatchBoth( NodeGo( node, e ))
      }
      case OSCNodeEndMessage( nodeID, _ ) => {
         nodes.get( nodeID ).foreach( node => {
            sync.synchronized {
               nodes -= node.id
            }
            dispatchBoth( NodeEnd( node, e ))
         })
      }
      case OSCNodeOffMessage( nodeID, _ ) => {
         nodes.get( nodeID ).foreach( node => {
            dispatchBoth( NodeOff( node, e ))
         })
      }
      case OSCNodeOnMessage( nodeID, _ ) => {
         nodes.get( nodeID ).foreach( node => {
            dispatchBoth( NodeOn( node, e ))
         })
      }
      case OSCNodeMoveMessage( nodeID, _ ) => {
         nodes.get( nodeID ).foreach( node => {
            dispatchBoth( NodeMove( node, e ))
         })
      }
      case _ =>
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