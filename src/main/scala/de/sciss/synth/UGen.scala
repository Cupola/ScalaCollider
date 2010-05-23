/*
 *  UGen.scala
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

import collection.immutable.{ IndexedSeq => IIdxSeq, Seq => ISeq }
import math._
import SynthGraph._

/**
 *    @version 0.15, 21-May-10
 */
trait UGenProxy {
   def source : UGen
   def outputIndex : Int
}

//trait ExclusiveUGen     // marker trait: UGen can only occur once in a synthdef

/**
 *    Marks a ugen which has side effects
 *    such as writing to a bus or buffer,
 *    communicating back to the client, etc.
 *    Only side-effect ugens are valid roots
 *    of the ugen graph, that way other
 *    orphaned non-side-effect ugens are
 *    automatically removed from the graph.
 */
trait SideEffectUGen {
   me: UGen =>

//   // ---- constructor ----
//   {
//      SynthGraph.builder.addUGen( this )
//   }
}

abstract class UGen
extends RatedGE with UGenProxy {
   // ---- constructor ----
   {
      SynthGraph.builder.addUGen( this )
   }

   def name = { val cn = getClass.getName; cn.substring( cn.lastIndexOf( '.' ) + 1 )}
   def displayName = name
//   def outputRates: Seq[ Rate ]
   def inputs: Seq[ UGenIn ]
   def numInputs = inputs.size
   def source = this
   def specialIndex = 0
   def outputIndex = 0

//   def checkInputs : Option[String] = {
//      // checkValidInputs
//      None
//   }
   
//   def optimizeGraph {
//      // nothing
//   }

   override def toString: String = {
      name + "." + rate.methodName + inputs.mkString( "(", ", ", ")" )
   }
}

// a class for UGens with multiple outputs
abstract class MultiOutUGen( outputRates: IIdxSeq[ Rate ], val inputs: Seq[ UGenIn ])
extends UGen {
   // most multi out ugens use the same rate for all outputs,
   // therefore we have a simpler constructor
   def this( rate: Rate, numOutputs: Int, inputs: Seq[ UGenIn ]) = this( Vector.fill( numOutputs )( rate ), inputs )
   
   final override def numOutputs = outputRates.size
	final def outputs: IIdxSeq[ UGenIn ] = outputRates.zipWithIndex.map(
      tup => UGenOutProxy( this, tup._2, tup._1 ))
}

abstract class ZeroOutUGen( val inputs: UGenIn* )
extends UGen with SideEffectUGen {
   final override def numOutputs = 0
   final def outputs = Vector.empty
}
