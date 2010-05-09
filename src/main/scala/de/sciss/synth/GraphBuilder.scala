/*
 *  GraphBuilder.scala
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

import SC._
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq, Seq => ISeq }
import ugen.{ BinaryOpUGen, EnvGen, MulAdd, Silent, Out, Poll, UnaryOpUGen }

/**
 * 	@version	0.14, 09-May-10
 */
trait GE {
   def outputs : IIdxSeq[ UGenIn ]
   def numOutputs : Int = outputs.size
   def `\\`( idx: Int ) : UGenIn = outputs( idx )

   import UnaryOpUGen._
   
   // unary ops
   def neg : GE               = Neg.make( this )
// def bitNot : GE	         = BitNot.make( this )
   def abs : GE	            = Abs.make( this )
// def toFloat : GE	         = UnOp.make( 'asFloat, this )
// def toInteger : GE	      = UnOp.make( 'asInteger, this )
   def ceil : GE	            = Ceil.make( this )
   def floor : GE	            = Floor.make( this )
   def frac : GE	            = Frac.make( this )
   def signum : GE	         = Signum.make( this )
   def squared : GE           = Squared.make( this )
   def cubed : GE             = Cubed.make( this )
   def sqrt : GE              = Sqrt.make( this )
   def exp : GE               = Exp.make( this )
   def reciprocal : GE        = Reciprocal.make( this )
   def midicps : GE           = Midicps.make( this )
   def cpsmidi : GE           = Cpsmidi.make( this )
   def midiratio : GE         = Midiratio.make( this )
   def ratiomidi : GE         = Ratiomidi.make( this )
   def dbamp : GE             = Dbamp.make( this )
   def ampdb : GE             = Ampdb.make( this )
   def octcps : GE            = Octcps.make( this )
   def cpsoct : GE            = Cpsoct.make( this )
   def log : GE               = Log.make( this )
   def log2 : GE              = Log2.make( this )
   def log10 : GE             = Log10.make( this )
   def sin : GE               = Sin.make( this )
   def cos : GE               = Cos.make( this )
   def tan : GE               = Tan.make( this )
   def asin : GE              = Asin.make( this )
   def acos : GE              = Acos.make( this )
   def atan : GE              = Atan.make( this )
   def sinh : GE              = Sinh.make( this )
   def cosh : GE              = Cosh.make( this )
   def tanh : GE              = Tanh.make( this )
// def rand : GE              = UnOp.make( 'rand, this )
// def rand2 : GE             = UnOp.make( 'rand2, this )
// def linrand : GE           = UnOp.make( 'linrand, this )
// def bilinrand : GE         = UnOp.make( 'bilinrand, this )
// def sum3rand : GE          = UnOp.make( 'sum3rand, this )
   def distort : GE           = Distort.make( this )
   def softclip : GE          = Softclip.make( this )
// def coin : GE              = UnOp.make( 'coin, this )
// def even : GE              = UnOp.make( 'even, this )
// def odd : GE               = UnOp.make( 'odd, this )
// def rectWindow : GE        = UnOp.make( 'rectWindow, this )
// def hanWindow : GE         = UnOp.make( 'hanWindow, this )
// def welWindow : GE         = UnOp.make( 'sum3rand, this )
// def triWindow : GE         = UnOp.make( 'triWindow, this )
   def ramp : GE              = Ramp.make( this )
   def scurve : GE            = Scurve.make( this )
// def isPositive : GE        = UnOp.make( 'isPositive, this )
// def isNegative : GE        = UnOp.make( 'isNegative, this )
// def isStrictlyPositive : GE= UnOp.make( 'isStrictlyPositive, this )
// def rho : GE               = UnOp.make( 'rho, this )
// def theta : GE             = UnOp.make( 'theta, this )

   import BinaryOpUGen._

   // binary ops
   def +( b: GE ) : GE        = Plus.make( this, b )
   def -( b: GE ) : GE        = Minus.make( this, b )
   def *( b: GE ) : GE        = Times.make( this, b )
// def div( b: GE ) : GE      = IDiv.make( this, b )
   def /( b: GE ) : GE        = Div.make( this, b )
   def %( b: GE ) : GE        = Mod.make( this, b )
   def ===( b: GE ) : GE      = Eq.make( this, b )
   def !==( b: GE ) : GE      = Neq.make( this, b )
   def <( b: GE ) : GE	      = Lt.make( this, b )
   def >( b: GE ) : GE	      = Gt.make( this, b )
   def <=( b: GE ) : GE	      = Leq.make( this, b )
   def >=( b: GE ) : GE	      = Geq.make( this, b )
   def min( b: GE ) : GE      = Min.make( this, b )
   def max( b: GE ) : GE      = Max.make( this, b )
   def &( b: GE ) : GE	      = BitAnd.make( this, b )
   def |( b: GE ) : GE	      = BitOr.make( this, b )
   def ^( b: GE ) : GE	      = BitXor.make( this, b )
// def Lcm( b: GE ) : GE      = Lcm.make( this, b )
// def Gcd( b: GE ) : GE      = Gcd.make( this, b )
   def round( b: GE ) : GE    = Round.make( this, b )
   def roundup( b: GE ) : GE  = Roundup.make( this, b ) // sclang uses camel case instead
   def trunc( b: GE ) : GE    = Trunc.make( this, b )
   def atan2( b: GE ) : GE    = Atan2.make( this, b )
   def hypot( b: GE ) : GE    = Hypot.make( this, b )
   def hypotx( b: GE ) : GE   = Hypotx.make( this, b )
   def pow( b: GE ) : GE      = Pow.make( this, b )
// def <<( b: GE ) : GE       = <<.make( this, b )
// def >>( b: GE ) : GE       = >>.make( this, b )
// def unsgnRghtShift( b: GE ) : GE = UnsgnRghtShift.make( this, b )
// def fill( b: GE ) : GE     = Fill.make( this, b )
   def ring1( b: GE ) : GE    = Ring1.make( this, b )
   def ring2( b: GE ) : GE    = Ring2.make( this, b )
   def ring3( b: GE ) : GE    = Ring3.make( this, b )
   def ring4( b: GE ) : GE    = Ring4.make( this, b )
   def difsqr( b: GE ) : GE   = Difsqr.make( this, b )
   def sumsqr( b: GE ) : GE   = Sumsqr.make( this, b )
   def sqrsum( b: GE ) : GE   = Sqrsum.make( this, b )
   def sqrdif( b: GE ) : GE   = Sqrdif.make( this, b )
   def absdif( b: GE ) : GE   = Absdif.make( this, b )
   def thresh( b: GE ) : GE   = Thresh.make( this, b )
   def amclip( b: GE ) : GE   = Amclip.make( this, b )
   def scaleneg( b: GE ) : GE = Scaleneg.make( this, b )
   def clip2( b: GE ) : GE    = Clip2.make( this, b )
   def excess( b: GE ) : GE   = Excess.make( this, b )
   def fold2( b: GE ) : GE    = Fold2.make( this, b )
   def wrap2( b: GE ) : GE    = Wrap2.make( this, b )
   def firstarg( b: GE ) : GE = Firstarg.make( this, b ) // sclang uses camel case instead
// def rrand( b: GE ) : GE    = Rrand.make( this, b )
// def exprrand( b: GE ) : GE = Exprrand.make( this, b )

   def madd( mul: GE, add: GE ) : GE = {
      Rates.highest( outputs.map( _.rate ): _* ) match {
         case `audio`   => MulAdd.ar( this, mul, add )
         case `control` => MulAdd.kr( this, mul, add )
         case r         => error( "Illegal rate " + r )
      }
   }

   def poll( trig: GE, label: String, trigID: GE = -1 ) : GE = {
      import GraphBuilder._

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

case class UGenInSeq( outputs: IIdxSeq[ UGenIn ]) extends GE {
// def numOutputs = outputs.size
// def getOutputAt( idx: Int ) = elements( idx )
// def outputs: Seq[ UGenIn ] = elements

   override def toString = outputs.mkString( "[ ", ", ", " ]" )
}

object GraphBuilder {
	//used to create an out ugen automatically and a fade envelope

   def seq( elements: IIdxSeq[ UGenIn ]) : GE = {
      if( elements.size == 1 ) elements.head else new UGenInSeq( elements )
   }
     
   def wrapOut( name: String, thunk: => GE, fadeTime: Option[Float] ) : SynthDef = {
      SynthDef( name ) {
         val res1 = thunk
         val rate = Rates.highest( res1.outputs.map( _.rate ): _* )
         if( (rate == audio) || (rate == control) ) {
            val res2 = fadeTime.map( fdt => makeFadeEnv( fdt ) * res1 ) getOrElse res1
            val i_out = "i_out".ir
            if( rate == audio ) {
               Out.ar( i_out, res2 )
            } else {
               Out.kr( i_out, res2 )
            }
         } else res1
      }
	}

	def makeFadeEnv( fadeTime: Float ) : GE = {
		val dt			= "fadeTime".kr( fadeTime )
		val gate       = "gate".kr( 1 )
		val startVal	= (dt <= 0)
      // this is slightly more costly than what sclang does
      // (using non-linear shape plus an extra unary op),
      // but it fadeout is much smoother this way...
		EnvGen.kr( Env( startVal, List( EnvSeg( 1, 1, curveShape( -4 )), EnvSeg( 1, 0, sineShape )), 1 ),
         gate, timeScale = dt, doneAction = freeSelf ).squared
	}

   def expand( args: GE* ): Seq[ List[ UGenIn ]] = {
      var chanExp = 0
      var allOne  = true
      var hasZero = false
      for( arg <- args ) {
         chanExp = math.max( chanExp, arg.numOutputs ) // shitty implicits don't work properly
         allOne  = allOne && (arg.numOutputs == 1)
         hasZero = hasZero || (arg.numOutputs == 0)
      }
//    println( "chanExp " + chanExp + "; allOne " + allOne + "; hasZero " + hasZero )
      if( allOne ) {
         List( args.toList.flatMap( _.outputs.toList ))
      } else if( hasZero ) {
         Nil	// cannot wrap zero size seq
      } else {
         val exp = args.toList.map( _.outputs.toArray )
         val test1 = exp.toList
//         val res = for( ch <- 0 until chanExp ) yield exp.map( (arr) => arr.apply( ch % arr.size ))
         val res = for( ch <- 0 until chanExp ) yield {
            exp.map(
               (arr) => {
                  val res = arr.apply( ch % arr.size )
                  res
               }
            )
         }
         res
      }
   }

   def simplify( res: Seq[ GE ]) : GE = { // UGenIn
//    println( "simplify : " + res )
      if( res.size == 1 ) {
         res.head
      } else {
         seqOfGEToGE( res )
      }
   }

   def replaceZeroesWithSilence( ge: GE ) : GE = {
      val ins = ge.outputs
      val numZeroes = ins.foldLeft( 0 )( (sum, in) => in match {
         case Constant( 0 ) => sum + 1
         case _ => sum
      })
      if( numZeroes == 0 ) {
         ge
      } else {
         val silent = Silent.ar( numZeroes ).outputs.iterator
         val res = ins map (in => in match {
            case Constant( 0 ) => silent.next
            case _ => in
         })
         simplify( res )
      }
   }
}