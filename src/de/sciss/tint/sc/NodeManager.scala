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

/*
import _root_.edu.uci.ics.jung.graph.{ DelegateTree, DirectedGraph, DirectedSparseGraph,
	Graph, ObservableGraph, Tree }
import _root_.edu.uci.ics.jung.graph.util.Graphs
*/

import scala.collection.immutable.IntMap
import scala.collection.mutable.ListBuffer

/**
 *	@version	0.11, 18-Jan-10
 *	@author		Hanns Holger Rutz
 */
object NodeManager {
    case class NodeGo(  node: Node, msg: OSCNodeChange )
    case class NodeEnd( node: Node, msg: OSCNodeChange )
    case class NodeOn(  node: Node, msg: OSCNodeChange )
    case class NodeOff( node: Node, msg: OSCNodeChange )
    case class NodeMove( node: Node, msg: OSCNodeChange )
}

class NodeManager( server: Server ) extends Model {

    import NodeManager._
    
/*
	val graph: DirectedGraph[ Node, Long ] = Graphs.synchronizedDirectedGraph( new DirectedSparseGraph() )
	val ograph = new ObservableGraph( graph )
    */
	private var nodes = IntMap.empty[ Node ]
	
	private var autoAdd = true
	
	// ---- constructor ----
	{
		val rootNode = server.rootNode // new Group( server, 0 )
/*
		ograph.addVertex( rootNode )
*/
		nodes += rootNode.id -> rootNode
//		val baseNode = server.defaultGroup // new Group( server, 1 )
//		dtree.addChild( (rootNode.id.toLong << 32) | (baseNode.id.toLong & 0xFFFFFFFF), rootNode, baseNode )
/*
		ograph.addVertex( baseNode )
		ograph.addEdge( (rootNode.id.toLong << 32) | (baseNode.id.toLong & 0xFFFFFFFF), rootNode, baseNode )
*/
//		nodes += baseNode.id -> baseNode
		// XXX
//		dtree.addChild( edgeID, parent, node )
	}

	def nodeChange( e: OSCNodeChange ) {
		val nodeO = nodes.get( e.nodeID )
		val node  = if( nodeO.isDefined ) {
			nodeO.get
		} else if( autoAdd && (e.name == "/n_go") && nodes.contains( e.parentID )) {
			val created = e match {
				case ee: OSCSynthChange => new Synth( "?", server, e.nodeID ) // que se puede acer...
				case ee: OSCGroupChange => new Group( server, e.nodeID )
			}
			nodes += created.id -> created
            /*
			ograph.addVertex( created )
			ograph.addEdge( (e.parentID.toLong << 32) | (e.nodeID.toLong & 0xFFFFFFFF), nodes( e.parentID ), created )
            */
			created
		} else return
		
		e.name match {
			case "/n_go"   => nodeGo( node, e )
			case "/n_end"  => nodeEnd( node, e )
			case "/n_off"  => nodeOff( node, e )
			case "/n_on"   => nodeOn( node, e )
			case "/n_move" => nodeMove( node, e )
			case _ =>
		}
		
//		listeners.foreach( _.apply( this, node, e ))
	}

	private def nodeGo( node: Node, e: OSCNodeChange ) {
		val parentO = nodes.get( e.parentID )
		
		parentO.foreach( parent => {
			val edgeID = (parent.id.toLong << 32) | (node.id.toLong & 0xFFFFFF)
/*			if( !ograph.containsVertex( node )) {
				ograph.addVertex( node )
				ograph.addEdge( edgeID, parent, node )
			}
*/		})

        dispatch( NodeGo( node, e ))
	}
	
	private def nodeEnd( node: Node, e: OSCNodeChange ) {
		val parentO = nodes.get( e.parentID )
		
		nodes -= node.id
		
		parentO.foreach( parent => {
			val edgeID = (parent.id.toLong << 32) | (node.id.toLong & 0xFFFFFF)
		})
/*		ograph.removeVertex( node )*/

        dispatch( NodeEnd( node, e ))
	}

	private def nodeOff( node: Node, e: OSCNodeChange ) {
        dispatch( NodeOff( node, e ))
	}

	private def nodeOn( node: Node, e: OSCNodeChange ) {
        dispatch( NodeOn( node, e ))
	}

	private def nodeMove( node: Node, e: OSCNodeChange ) {
        dispatch( NodeMove( node, e ))
	}
	
	// eventually this should be done automatically
	// by the message dispatch management
	def register( node: Node ) {
		nodes += node.id -> node
	}
	
	def unregister( node: Node ) {
		nodes -= node.id
	}
}
