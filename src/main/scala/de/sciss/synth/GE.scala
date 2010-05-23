package de.sciss.synth

import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq }
import ugen.{BinaryOpUGen, MulAdd, Poll}

/**
 *    The UGen graph is constructed from interconnecting graph elements (GE).
 *    Graph elements can be decomposed into a sequence of UGenIn objects.
 *    Graph elements are ordinary UGens, UGen proxies, Control proxies,
 *    Constants, and collections of UGen inputs which result from
 *    multichannel expansion (UGenInSeq). 
 *
 *    @version 0.11, 23-May-10
 */
trait GE {
   def outputs : IIdxSeq[ UGenIn ]
   def numOutputs : Int = outputs.size
   def `\\`( idx: Int ) : UGenIn = outputs( idx )

   private[synth] def ops = new GEOps( this )

   // special binop handling
   def +( b: GE ) : GE        = BinaryOpUGen.Plus.make( this, b )

   def madd( mul: GE, add: GE ) : GE = {
      Rate.highest( outputs.map( _.rate ): _* ) match {
         case `audio`   => MulAdd.ar( this, mul, add )
         case `control` => MulAdd.kr( this, mul, add )
         case `scalar`  => this * mul + add
         case r         => error( "Illegal rate " + r )
      }
   }

   def poll( trig: GE, label: String, trigID: GE = -1 ) : GE = {
      import SynthGraph._

      val inputs  = this.outputs
      val numIns  = inputs.size
      val trigs   = trig.outputs
      val ids     = trigID.outputs
      val labels  = if( label != null ) {
         Vector.fill( numIns )( label )
      } else {
         val multi = numIns > 1
         inputs.zipWithIndex.map( tup => {
            val (in, ch) = tup
            (if( multi ) ch.toString + " -> " else "") + (in match {
               case p: UGenProxy => if( p.source.numOutputs > 1 ) "(" + p.outputIndex + ")" else ""
               case x            => x.toString
            })
         })
      }
      val numExp = math.max( numIns, math.max( trigs.size, math.max( labels.size, ids.size )))
      seq( (0 until numExp).flatMap[ UGenIn, IIdxSeq[ UGenIn ]]( ch => (inputs( ch ).rate match {
         case `audio` => Poll.ar( trigs( ch ), inputs( ch ), labels( ch ), ids( ch ))
         case _ =>       Poll.kr( trigs( ch ), inputs( ch ), labels( ch ), ids( ch ))
      }).outputs )( breakOut ))
   }
}
