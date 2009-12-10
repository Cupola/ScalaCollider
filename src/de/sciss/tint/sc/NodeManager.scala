/*
 *  NodeManager.scala
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

import _root_.edu.uci.ics.jung.graph.{ DelegateTree, DirectedGraph, DirectedSparseGraph,
	Graph, ObservableGraph, Tree }
import _root_.edu.uci.ics.jung.graph.util.Graphs

import _root_.scala.collection.immutable.IntMap
import _root_.scala.collection.mutable.ListBuffer

/**
 *	@version	0.10, 27-Nov-09
 *	@author		Hanns Holger Rutz
 */
class NodeManager( server: Server ) {
	
//	private var incrEdgeID = 0
	val graph: DirectedGraph[ Node, Long ] = Graphs.synchronizedDirectedGraph( new DirectedSparseGraph() )
//	private val dtree = new DelegateTree[ Node, Long ]()
//	val graph: Tree[ Node, Long ] = Graphs.synchronizedTree( dtree )
	val ograph = new ObservableGraph( graph )
	private var nodes = IntMap.empty[ Node ]
//	private val	listeners = new ListBuffer[ (NodeManager, Node, OSCNodeChange) => Unit ]()
	
	private var autoAdd = true
	
//	def graph: DirectedGraph[ Node, Long ] = graph
//	def ograph: ObservableGraph = ograph
	
	// ---- constructor ----
	{
		val rootNode = new Group( server, 0 )
		ograph.addVertex( rootNode )
		nodes += rootNode.id -> rootNode
		val baseNode = new Group( server, 1 )
//		dtree.addChild( (rootNode.id.toLong << 32) | (baseNode.id.toLong & 0xFFFFFFFF), rootNode, baseNode )
		ograph.addVertex( baseNode )
		ograph.addEdge( (rootNode.id.toLong << 32) | (baseNode.id.toLong & 0xFFFFFFFF), rootNode, baseNode )
		nodes += baseNode.id -> baseNode
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
//			ograph.addChild( (e.parentID.toLong << 32) | (e.nodeID.toLong & 0xFFFFFFFF), nodes( e.parentID ), created )
			ograph.addVertex( created )
			ograph.addEdge( (e.parentID.toLong << 32) | (e.nodeID.toLong & 0xFFFFFFFF), nodes( e.parentID ), created )
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
	
//	def graph: ObservableGraph[ Node, Long ] = graph

//	def addListener( func: (NodeManager, Node, OSCNodeChange) => Unit ) {
//    	listeners += func
//	}
// 
//	def removeListener( func: (NodeManager, Node, OSCNodeChange) => Unit ) {
//    	listeners -= func
//	}
	
	private def nodeGo( node: Node, e: OSCNodeChange ) {
		val parentO = nodes.get( e.parentID )
		
//		ograph.addVertex( node )
		parentO.foreach( parent => {
//			val edgeID = incrEdgeID // XXX sync
//			incrEdgeID += 1
			val edgeID = (parent.id.toLong << 32) | (node.id.toLong & 0xFFFFFF)
//			ograph.addEdge( edgeID, parent, node )
			if( !ograph.containsVertex( node )) {
//				dtree.addChild( edgeID, parent, node )
				ograph.addVertex( node )
				ograph.addEdge( edgeID, parent, node )
			}
		})
		
//		val group	= (Group) mapNodes.get( e.parentID )
//		val pred	= (Node) mapNodes.get( e.predID )
//		val succ	= (Node) mapNodes.get( e.succID )
//		
//		node.setGroup( group );
//		node.setPredNode( pred );
//		node.setSuccNode( succ );
//		if( pred != null ) pred.setSuccNode( node );
//		if( succ != null ) succ.setPredNode( node );
		
//		if( group != null ) {
//			if( e.getPredNodeID() == -1 ) {
//				group.setHeadNode( node );
//			}
//			if( e.getSuccNodeID() == -1 ) {
//				group.setTailNode( node );
//			}
//		}
	}
	
	private def nodeEnd( node: Node, e: OSCNodeChange ) {
		val parentO = nodes.get( e.parentID )
		
		nodes -= node.id
		
		parentO.foreach( parent => {
			val edgeID = (parent.id.toLong << 32) | (node.id.toLong & 0xFFFFFF)
//			ograph.removeEdge( edgeID )
		})
//		dtree.removeChild( node )
		ograph.removeVertex( node )
	}

	private def nodeOff( node: Node, e: OSCNodeChange ) {
		
	}

	private def nodeOn( node: Node, e: OSCNodeChange ) {
		
	}

	private def nodeMove( node: Node, e: OSCNodeChange ) {
		
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
