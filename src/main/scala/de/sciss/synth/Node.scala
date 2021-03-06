/*
 *  Node.scala
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

import osc.{ OSCGroupHeadMessage, OSCGroupTailMessage, OSCNodeAfterMessage, OSCNodeBeforeMessage, OSCNodeFillInfo,
             OSCNodeFillMessage, OSCNodeFreeMessage, OSCNodeMapMessage, OSCNodeMapaMessage, OSCNodeMapanMessage,
             OSCNodeMapnMessage, OSCNodeRunMessage, OSCNodeSetMessage, OSCNodeSetnMessage, OSCNodeTraceMessage }

/**
 * Add-actions are used by the server to determine where to place a node with
 * respect to other nodes. They form an enumeration of integers which are
 * represented by case objects being subclasses of this abstract class.
 *
 * @see  [[de.sciss.synth.Synth]]
 * @see  [[de.sciss.synth.Group]]
 */
sealed abstract class AddAction( val id: Int )

/**
 * AddAction with id 0, indicating that a node should be add to the head of
 * of a target group.
 */
case object addToHead   extends AddAction( 0 )
/**
 * AddAction with id 1, indicating that a node should be add to the tail of
 * of a target group.
 */
case object addToTail   extends AddAction( 1 )
/**
 * AddAction with id 2, indicating that a node should be added to the same
 * group as the target node, right before it.
 */
case object addBefore   extends AddAction( 2 )
/**
 * AddAction with id 3, indicating that a node should be added to the same
 * group as the target node, right after it.
 */
case object addAfter    extends AddAction( 3 )
/**
 * AddAction with id 4, indicating that a node should replace an existing
 * node, that is take the target node's exact position in the tree.
 */
case object addReplace  extends AddAction( 4 )

/**
 *    @version    0.14, 01-Jul-10
 */
abstract class Node extends Model {
   import Model._

   // ---- abstract ----
   val server: Server
   val id: Int

//	var group : Group = null
//	var isPlaying	= false
//	var isRunning  = false

//	def register: Unit = register( false )
	def register {
//  	NodeWatcher.register( this, assumePlaying )
       server.nodeMgr.register( this )
  	}

   def onGo( thunk: => Unit ) {
      register
      lazy val l: Listener = {
         case NodeManager.NodeGo( _, _ ) => {
            removeListener( l )
            thunk
         }
      }
      addListener( l )
   }

   def onEnd( thunk: => Unit ) {
      register
      lazy val l: Listener = {
         case NodeManager.NodeEnd( _, _ ) => {
            removeListener( l )
            thunk
         }
      }
      addListener( l )
   }

   protected[synth] def updated( change: NodeManager.NodeChange ) {
      // XXX need to update isPlaying, isRunning etc.
      dispatch( change )
   }

//	def free : Node = free( true )

	def free {
  		server ! freeMsg
//  		group = null
//  		isPlaying = false
//  		isRunning = false
//  		this
  	}
  
//  private def asArray( o: Object ) : Array[Object] = Seq( o ).toArray
//  private def asArray( o: Int ) : Array[Object] = Seq( o.asInstanceOf[AnyRef] ).toArray
  
  	def freeMsg = OSCNodeFreeMessage( id )

  	def run : Unit = run( true )
  
  	def run( flag: Boolean ) {
  		server ! runMsg( flag )
  		this
  	}
	
  	def runMsg : OSCNodeRunMessage = runMsg( true )
  	def runMsg( flag: Boolean ) = OSCNodeRunMessage( id -> flag )
  
  	def set( pairs: ControlSetMap* ) {
  		server ! setMsg( pairs: _* )
  	}
	
  	def setMsg( pairs: ControlSetMap* ) =
  		OSCNodeSetMessage( id, pairs: _* )

  	def setn( pairs: ControlSetMap* ) {
  		server ! setnMsg( pairs: _* )
  	}
	
  	def setnMsg( pairs: ControlSetMap* ) =
  		OSCNodeSetnMessage( id, pairs: _* )

  	def trace {
  		server ! traceMsg
  	}

   def traceMsg = OSCNodeTraceMessage( id )

  	def release : Unit = release( None )
   def release( releaseTime: Float ) {  release( Some( releaseTime ))}
   def release( releaseTime: Double ) { release( Some( releaseTime.toFloat ))}

  	def release( releaseTime: Option[ Float ]) {
  		server ! releaseMsg( releaseTime )
  	}

  	def releaseMsg : OSCNodeSetMessage = releaseMsg( None )
   def releaseMsg( releaseTime: Float ) : OSCNodeSetMessage = releaseMsg( Some( releaseTime ))

  	// assumes a control called 'gate' in the synth
  	def releaseMsg( releaseTime: Option[ Float ]) = {
  		val value = releaseTime.map( -1.0f - _ ).getOrElse( 0.0f )
  		setMsg( "gate" -> value )
	}

   def map( pairs: SingleControlKBusMap* ) {
      server ! mapMsg( pairs: _* )
   }

   def mapMsg( pairs: SingleControlKBusMap* ) =
      OSCNodeMapMessage( id, pairs: _* )
   
  	def mapn( mappings: ControlKBusMap* ) {
  		server ! mapnMsg( mappings: _* )
  	}
  	
  	def mapnMsg( mappings: ControlKBusMap* ) =
  		OSCNodeMapnMessage( id, mappings: _* )

   def mapa( pairs: SingleControlABusMap* ) {
      server ! mapaMsg( pairs: _* )
   }

   def mapaMsg( pairs: SingleControlABusMap* ) =
      OSCNodeMapaMessage( id, pairs: _* )

  	def mapan( mappings: ControlABusMap* ) {
  		server ! mapanMsg( mappings: _* )
  	}

  	def mapanMsg( mappings: ControlABusMap* ) =
  		OSCNodeMapanMessage( id, mappings: _* )

   def fill( control: Any, numChannels: Int, value: Float ) {
      server ! fillMsg( control, numChannels, value )
   }

  	def fill( fillings: OSCNodeFillInfo* ) {
  		server ! fillMsg( fillings: _* )
  	}
	
   def fillMsg( control: Any, numChannels: Int, value: Float ) =
      OSCNodeFillMessage( id, OSCNodeFillInfo( control, numChannels, value ))
   
  	def fillMsg( fillings: OSCNodeFillInfo* ) = OSCNodeFillMessage( id, fillings: _* )

   /**
    * Moves this node before another node
    *
    * @param   node  the node before which to move this node
    *
    * @see  [[de.sciss.synth.osc.OSCNodeBeforeMessage]]
    */
   def moveBefore( node: Node ) { server ! moveBeforeMsg( node )}
   /**
    * Creates an OSC message to move this node before another node
    *
    * @param   node  the node before which to move this node
    *
    * @see  [[de.sciss.synth.osc.OSCNodeBeforeMessage]]
    */
   def moveBeforeMsg( node: Node )  = OSCNodeBeforeMessage( id -> node.id )

   /**
    * Moves this node after another node
    *
    * @param   node  the node after which to move this node
    *
    * @see  [[de.sciss.synth.osc.OSCNodeAfterMessage]]
    */
   def moveAfter( node: Node ) { server ! moveAfterMsg( node )}
   /**
    * Creates an OSC message to move this node after another node
    *
    * @param   node  the node after which to move this node
    *
    * @see  [[de.sciss.synth.osc.OSCNodeAfterMessage]]
    */
   def moveAfterMsg( node: Node )   = OSCNodeAfterMessage( id -> node.id )

   def moveToHead( group: Group ) { server ! moveToHeadMsg( group )}
  	def moveToHeadMsg( group: Group ) : OSCGroupHeadMessage = group.moveNodeToHeadMsg( this )

   def moveToTail( group: Group ) { server ! moveToTailMsg( group )}
  	def moveToTailMsg( group: Group ) : OSCGroupTailMessage = group.moveNodeToTailMsg( this )
}

//class NodeRef( val server: Server, val id: Int ) extends Node
