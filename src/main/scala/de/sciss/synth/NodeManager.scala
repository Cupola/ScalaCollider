/*
 *  NodeManager.scala
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

import collection.immutable.IntMap
import osc._

/**
 *    @version 0.12, 10-May-10
 */
object NodeManager {
   abstract sealed class NodeChange { def node: Node; def info: OSCNodeInfo }
   case class NodeGo(   node: Node, info: OSCNodeInfo ) extends NodeChange
   case class NodeEnd(  node: Node, info: OSCNodeInfo ) extends NodeChange
   case class NodeOn(   node: Node, info: OSCNodeInfo ) extends NodeChange
   case class NodeOff(  node: Node, info: OSCNodeInfo ) extends NodeChange
   case class NodeMove( node: Node, info: OSCNodeInfo ) extends NodeChange
   case object Cleared
}

class NodeManager( server: Server ) extends Model {

   import NodeManager._
    
	private var nodes: IntMap[ Node ] = _
	private var autoAdd  = true
   private val sync     = new AnyRef
	
	// ---- constructor ----
	{
      clear
//      if( server.isRunning ) {
//         val defaultGroup = server.defaultGroup
//         nodes += defaultGroup.id -> defaultGroup
//      }
	}

	def nodeChange( e: OSCNodeChange ) : Unit = e match {
      case OSCNodeGoMessage( nodeID, info ) => {
         val node = nodes.get( nodeID ) getOrElse {
            if( autoAdd && nodes.contains( info.parentID )) {
               val created = info match {
                  case ee: OSCSynthInfo => new Synth( server, nodeID )
                  case ee: OSCGroupInfo => new Group( server, nodeID )
               }
               register( created )
               created
            } else return
         }
         dispatchBoth( NodeGo( node, info ))
      }
      case OSCNodeEndMessage( nodeID, info ) => {
         nodes.get( nodeID ).foreach( node => {
            unregister( node )
            dispatchBoth( NodeEnd( node, info ))
         })
      }
      case OSCNodeOffMessage( nodeID, info ) => {
         nodes.get( e.nodeID ).foreach( node => {
            dispatchBoth( NodeOff( node, info ))
         })
      }
      case OSCNodeOnMessage( nodeID, info ) => {
         nodes.get( e.nodeID ).foreach( node => {
            dispatchBoth( NodeOn( node, info ))
         })
      }
      case OSCNodeMoveMessage( nodeID, info ) => {
         nodes.get( e.nodeID ).foreach( node => {
            dispatchBoth( NodeMove( node, info ))
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

   def getNode( id: Int ) : Option[ Node ] = sync.synchronized { nodes.get( id )}

   def clear {
      val rootNode = server.rootNode // new Group( server, 0 )
      sync.synchronized {
         nodes = IntMap( rootNode.id -> rootNode )
      }
      dispatch( Cleared )
   }
}