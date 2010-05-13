/*
 *  SC.scala
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

import de.sciss.scalaosc.{ OSCMessage }
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq }
import math._

/**
 * 	@version	0.14, 09-May-10
 */
object SC {
   // GEs
   implicit def floatToGE( x: Float ) = Constant( x )
   implicit def intToGE( x: Int ) = Constant( x.toFloat )
   implicit def doubleToGE( x: Double ) = Constant( x.toFloat )
   implicit def seqOfGEToGE( x: Seq[ GE ]) = new UGenInSeq( x.flatMap( _.outputs )( breakOut ))
   implicit def doneActionToGE( x: DoneAction ) = Constant( x.id )

   // ...und zurueck
   implicit def constantToFloat( c: Constant ) = c.value

   // why these are necessary now??
   implicit def seqOfFloatToGE( x: Seq[ Float ]) = new UGenInSeq( x.map( Constant( _ ))( breakOut ))
   implicit def seqOfIntToGE( x: Seq[ Int ]) = new UGenInSeq( x.map( i => Constant( i.toFloat ))( breakOut ))
   implicit def seqOfDoubleToGE( x: Seq[ Double ]) = new UGenInSeq( x.map( d => Constant( d.toFloat ))( breakOut ))

   // control mapping
   implicit def intFloatControlSet( tup: (Int, Float) )                    = SingleControlSetMap( tup._1, tup._2 )
   implicit def intIntControlSet( tup: (Int, Int) )                        = SingleControlSetMap( tup._1, tup._2.toFloat )
   implicit def intDoubleControlSet( tup: (Int, Double) )                  = SingleControlSetMap( tup._1, tup._2.toFloat )
   implicit def stringFloatControlSet( tup: (String, Float) )              = SingleControlSetMap( tup._1, tup._2 )
   implicit def stringIntControlSet( tup: (String, Int) )                  = SingleControlSetMap( tup._1, tup._2.toFloat )
   implicit def stringDoubleControlSet( tup: (String, Double) )            = SingleControlSetMap( tup._1, tup._2.toFloat )
   implicit def intFloatsControlSet( tup: (Int, IIdxSeq[ Float ]))         = MultiControlSetMap( tup._1, tup._2 )
   implicit def stringFloatsControlSet( tup: (String, IIdxSeq[ Float ]))   = MultiControlSetMap( tup._1, tup._2 )

   implicit def intIntControlBus( tup: (Int, Int) )               = SingleControlBusMap( tup._1, tup._2 )
   implicit def stringIntControlBus( tup: (String, Int) )         = SingleControlBusMap( tup._1, tup._2 )
   implicit def intIntIntControlBus( tup: (Int, Int, Int) )       = MultiControlBusMap( tup._1, tup._2, tup._3 )
   implicit def stringIntIntControlBus( tup: (String, Int, Int) ) = MultiControlBusMap( tup._1, tup._2, tup._3 )
   implicit def intBusControlBus( tup: (Int, ControlBus) )        = MultiControlBusMap( tup._1, tup._2.index, tup._2.numChannels )
   implicit def stringBusControlBus( tup: (String, ControlBus) )  = MultiControlBusMap( tup._1, tup._2.index, tup._2.numChannels )

   // pimping
   implicit def stringToControlFactory( name: String ) = new ControlFactory( name )
   implicit def thunkToGraphFunction[ T <% GE ]( thunk: => T ) = new GraphFunction( thunk )

//   // Misc
//   implicit def stringToOption( x: String ) = Some( x )

   // Buffer convenience
//   implicit def actionToCompletion( fun: Buffer => Unit ) : Buffer.Completion = Buffer.action( fun )
//   import Buffer.{ Completion => Comp }
   def message[T]( msg: => OSCMessage ) = Completion[T]( Some( _ => msg ), None )
   def message[T]( msg: T => OSCMessage ) = Completion[T]( Some( msg ), None )
   def action[T]( action: => Unit ) = Completion[T]( None, Some( _ => action ))
   def action[T]( action: T => Unit ) = Completion[T]( None, Some( action ))
   def complete[T]( msg: => OSCMessage, action: => Unit ) = Completion[T]( Some( _ => msg ), Some( _ => action ))
   def complete[T]( msg: T => OSCMessage, action: => Unit ) = Completion[T]( Some( msg ), Some( _ => action ))
   def complete[T]( msg: => OSCMessage, action: T => Unit ) = Completion[T]( Some( _ => msg ), Some( action ))
   def complete[T]( msg: T => OSCMessage, action: T => Unit ) = Completion[T]( Some( msg ), Some( action ))
   implicit def messageToCompletion[T]( msg: OSCMessage ) = message[T]( msg )
   implicit def messageToOption( msg: OSCMessage ) = Some( msg )

   // Nodes
//   implicit def intToNode( id: Int ) : Node = new Group( Server.default, id )
   implicit def serverToGroup( s: Server ) : Group = s.defaultGroup

//  implicit def stringToStringOrInt( x: String ) = new StringOrInt( x )
//  implicit def intToStringOrInt( x: Int ) = new StringOrInt( x )
  
   // explicit methods
   def play( thunk: => GE ) : Synth = play()( thunk )
   def play( target: Node = Server.default.defaultGroup, outBus: Int = 0,
             fadeTime: Option[Float] = Some( 0.02f ),
             addAction: AddAction = addToHead )( thunk: => GE ) : Synth =
      new GraphFunction( thunk ).play( target, outBus, fadeTime, addAction )

   // String
   def warn( s: String ) : String = {
      println( "WARNING:\n" + s )
      s
   }
}