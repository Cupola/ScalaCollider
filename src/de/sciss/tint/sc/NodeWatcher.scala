/*
 *  NodeWatcher.scala
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

import _root_.java.awt.EventQueue
import _root_.de.sciss.scalaosc.{ OSCMessage, OSCPacket }

/**
 *	@version	0.11, 24-Nov-09
 *	@author		Hanns Holger Rutz
 *	@deprecated	superceded by NodeManager
 */
object NodeWatcher {
	private var all : scala.collection.immutable.Map[ Server, NodeWatcher ] =
		new scala.collection.immutable.HashMap[ Server, NodeWatcher ]()
 
	def register( node: Node ) : Unit = register( node, false )
	def register( node: Node, assumePlaying: Boolean ) {
		newFrom( node.server ).register( node, assumePlaying )
	}
 
	def newFrom( server: Server ) : NodeWatcher = {
	  all.getOrElse( server, {
		  val nw = new NodeWatcher( server )
		  all += server -> nw
		  nw.start
		  nw
	  })
	}
}

class NodeWatcher( val server: Server ) extends AnyRef
/* with OSCListener */ with Runnable {
	private var isWatching = false
    private var nodes : scala.collection.immutable.Map[ Int, Node ] =
//    	new scala.collection.immutable.IntMap[ Node ]()   -- is abstract -- why??
    	new scala.collection.immutable.HashMap[ Int, Node ]()

//    private val cmds 			= List( "/n_go", "/n_end", "/n_off", "/n_on" )

	private var dumpMode		= 'off
	private val sync			= new Object
	private val collQueue		= new scala.collection.mutable.ListBuffer[ OSCMessage ]()	// element = (OSCMessage) ; synchronized through 'sync'
	private var autoRegister	= false
//	private lazy val em			= new EventManager( this )
	private val listeners		= new scala.collection.mutable.ListBuffer[ NodeListener ]()
    
    private var responders = NodeEvent.VALID_CMDS.map( cmd => {
    	new OSCResponderNode( server, cmd, (msg, addr, when) => messageReceived( msg ))
    })
 
	def register( node: Node ) : Unit = register( node, false )
 
	def register( node: Node, assumePlaying: Boolean ) {
	  sync.synchronized {
		if( !server.isRunning ) {
			nodes = nodes.empty
			return
		}
  
		if( isWatching ) {
			if( assumePlaying && !nodes.contains( node.id )) {
			  node.isPlaying = true
			}
			nodes += node.id -> node
		}
//	  println( "NodeWatcher.register : not yet implemented!" )
	  }
	}

	def unregister( node: Node ) {
	  sync.synchronized {
		nodes -= node.id
	  }
	}
 
	def addListener( l: NodeListener ) {
	  sync.synchronized {
		  listeners += l
	  }
	}
 
	def removeListener( l: NodeListener ) {
	  sync.synchronized {
		  listeners -= l
	  }
	}
 
	def start {
	  sync.synchronized {
		if( !isWatching ) {
			responders.foreach(_.add)
			isWatching = true
		}
	  }
	}
 
	def stop {
	  sync.synchronized {
		if( isWatching ) {
			responders.foreach(_.remove)
			isWatching = false
			clear
		}
	  }
	}
 
	def clear {
	  sync.synchronized {
		nodes.foreach( entry => {
		    val node = entry._2
			node.isPlaying = false
			node.isRunning = false
// XXX
//			node.changed('n_end);
		})
		nodes = nodes.empty
	  }
	}
 
	private def messageReceived( msg: OSCMessage ) {
 		if( dumpMode == 'text ) {
			OSCPacket.printTextOn( server.c.codec, System.out /* Server.getPrintStream */, msg )
		}

		if( autoRegister ) {
			sync.synchronized {
				val invoke = collQueue.isEmpty
				collQueue += msg
				if( invoke ) EventQueue.invokeLater( this )
			}
			return
		}
	
		val nodeIDObj = msg( 0 ).asInstanceOf[ Number ].intValue
		
//  println( "-----NODE " + nodeIDObj )
  
		sync.synchronized {
			if( nodes.contains( nodeIDObj )) {
				val invoke = collQueue.isEmpty
//  println( "-----YEP " + invoke )
				collQueue += msg
				if( invoke ) EventQueue.invokeLater( this )
			}
		}
	}

	// @synchronization	has to be called with sync on sync
	private def nodeGo( node: Node, e: NodeEvent ) {
//		final Group group	= (Group) mapNodes.get( new Integer( e.getParentGroupID() ));
//		final Node	pred	= (Node) mapNodes.get( new Integer( e.getPredNodeID() ));
//		final Node	succ	= (Node) mapNodes.get( new Integer( e.getSuccNodeID() ));
//		
//		node.setGroup( group );
//		node.setPredNode( pred );
//		node.setSuccNode( succ );
//		if( pred != null ) pred.setSuccNode( node );
//		if( succ != null ) succ.setPredNode( node );
//		
//		if( group != null ) {
//			if( e.getPredNodeID() == -1 ) {
//				group.setHeadNode( node );
//			}
//			if( e.getSuccNodeID() == -1 ) {
//				group.setTailNode( node );
//			}
//		}

		node.isRunning = true
		node.isPlaying = true

//		if( VERBOSE ) System.err.println( "NodeWatcher.nodeGo( " + node + " )" );
	}

	// @synchronization	has to be called with sync on mapNodes
	private def nodeEnd( node: Node, e: NodeEvent ) {
//		final Group group	= node.getGroup();
//		final Node	pred	= node.getPredNode();
//		final Node	succ	= node.getSuccNode();
//	
//		node.setGroup( null );
//		node.setPredNode( null );
//		node.setSuccNode( null );
//		if( pred != null ) pred.setSuccNode( succ );
//		if( succ != null ) succ.setPredNode( pred );
//		
//		if( group != null ) {
//			if( (group.getHeadNode() != null) && (group.getHeadNode().getNodeID() == node.getNodeID()) ) {
//				group.setHeadNode( succ );
//			}
//			if( (group.getTailNode() != null) && (group.getTailNode().getNodeID() == node.getNodeID()) ) {
//				group.setTailNode( pred );
//			}
//		}

		node.isPlaying = false
		node.isRunning = false
		nodes -= node.id

//		if( VERBOSE ) System.err.println( "NodeWatcher.nodeEnd( " + node + " )" );
	}

	// @synchronization	has to be called with sync on sync
	private def nodeMove( node: Node, e: NodeEvent ) {
//		final Group oldGroup	= node.getGroup();
//		final Node	oldPred		= node.getPredNode();
//		final Node	oldSucc		= node.getSuccNode();
//	
//		final Group newGroup	= (Group) mapNodes.get( new Integer( e.getParentGroupID() ));
//		final Node	newPred		= (Node) mapNodes.get( new Integer( e.getPredNodeID() ));
//		final Node	newSucc		= (Node) mapNodes.get( new Integer( e.getSuccNodeID() ));
//
//		node.setGroup( newGroup );
//		node.setPredNode( newPred );
//		node.setSuccNode( newSucc );
//		// needs to be done before setting new pred/succ
//		if( oldPred != null ) oldPred.setSuccNode( oldSucc );
//		if( oldSucc != null ) oldSucc.setPredNode( oldPred );
//		if( newPred != null ) newPred.setSuccNode( node );
//		if( newSucc != null ) newSucc.setPredNode( node );
//
//		// needs to be done before setting new group
//		if( oldGroup != null ) {
//			if( (oldGroup.getHeadNode() != null) && (oldGroup.getHeadNode().getNodeID() == node.getNodeID()) ) {
//				oldGroup.setHeadNode( oldSucc );
//			}
//			if( (oldGroup.getTailNode() != null) && (oldGroup.getTailNode().getNodeID() == node.getNodeID()) ) {
//				oldGroup.setTailNode( oldPred );
//			}
//		}
//
//		if( newGroup != null ) {
//			if( e.getPredNodeID() == -1 ) {
//				newGroup.setHeadNode( node );
//			}
//			if( e.getSuccNodeID() == -1 ) {
//				newGroup.setTailNode( node );
//			}
//		}
//
//		if( VERBOSE ) System.err.println( "NodeWatcher.nodeMove( " + node + " )" );
	}
 
	def run {
		sync.synchronized {
			if( !isWatching ) return

			val when = System.currentTimeMillis

			collQueue.foreach( msg => {
				val nodeIDObj	= msg( 0 ).asInstanceOf[ Number ].intValue

//println( "-------RUN " + nodeIDObj )
    
				val nodeO		= nodes.get( nodeIDObj )
				if( nodeO.isEmpty ) {
				  return
//					if( autoRegister ) {
//						node = ((Number) msg.getArg( 4 )).intValue() == NodeEvent.GROUP ? (Node) Group.basicNew( server, nodeIDObj.intValue() ) : (Node) Synth.basicNew( null, server, nodeIDObj.intValue() );
//						register( node );
//					} else if( !fireAllNodes ) return;
				}
				val node	= nodeO.get
				val nde		= NodeEvent.fromOSCMessage( msg, this, when, node )
//println( "-------EVENT " + nde.getID )

				nde.getID match { 	// update the node's fields
				case NodeEvent.GO => nodeGo( node, nde )
				case NodeEvent.END => nodeEnd( node, nde )
				case NodeEvent.ON => node.isPlaying = true
				case NodeEvent.OFF => node.isPlaying = false
				case NodeEvent.MOVE => nodeMove( node, nde )
				case NodeEvent.INFO => nodeMove( node, nde )
				}
    
				// we are already in the event thread, so let's just call the listeners directly
				listeners.foreach( l => {
					try {
						l.nodeAction( nde )
					}
					catch { case e1 => e1.printStackTrace }
				})
			}) // for iter
			
			collQueue.clear
			
		} // sync
	} // run
 
//	def messageReceivedNONO( msg: OSCMessage, addr: java.net.SocketAddress, when: Long ) {
//	  val nodeO = nodes.get( msg.getArg( 0 ).asInstanceOf[ Number ].intValue )
//	  nodeO.foreach( node => {
//		  msg.getName match {
//		  	case "/n_go" => {
//		  		node.isPlaying = true
//		  		node.isRunning = true
//// XXX
////		  		node.changed('n_go)
//		  	}
//		  	case "/n_end" => {
//		  		unregister( node )
//		  		node.isPlaying = false
//		  		node.isRunning = false
//// XXX
////		  		node.changed('n_end);
//		  	}
//		  	case "/n_off" => {
//		  		node.isRunning = false;
//// XXX
////		  		node.changed('n_off);
//		  	}
//		  	case "/n_on" => {
//		  		node.isRunning = true;
//// XXX
////		  		node.changed('n_on);
//		  	}
//	  	}
//	})
//   }
}
