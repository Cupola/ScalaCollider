/*
 *  BasicOpUGen.scala
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

package de.sciss.synth.ugen

import de.sciss.synth.{ Constant => c, _ }
import SC._
import GraphBuilder._

import math._

/**
 *    @version 0.11, 27-Apr-10
 */
object MulAdd {
  def ar( in: GE, mul: GE, add: GE ) : GE = {
    simplify( for( List( i, m, a ) <- expand( in, mul, add )) yield this( audio, i, m, a ))
  }

  def kr( in: GE, mul: GE, add: GE ) : GE = {
    simplify( for( List( i, m, a ) <- expand( in, mul, add )) yield this( control, i, m, a ))
  }

  protected[synth] def make( in: GE, mul: GE, add: GE ) : GE = {
    simplify( for( List( i, m, a ) <- expand( in, mul, add )) yield make1( i, m, a ))
  }

  private def make1( in: UGenIn, mul: UGenIn, add: UGenIn ) : GE =
    make1( Rates.highest( in.rate, mul.rate, add.rate ), in, mul, add )

  private def make1( rate: Rate, in: UGenIn, mul: UGenIn, add: UGenIn ) : GE =
    (mul, add) match {
      case (c(0), _)     => add
      case (c(1), c(0))  => in
      case (c(-1), c(0)) => in.neg
      case (_, c(0))     => in * mul
      case (c(-1), _)    => add - in
      case (c(1), _)     => in + add
      case _             => this( rate, in, mul, add )
   }
}

case class MulAdd( rate: Rate, in: UGenIn, mul: UGenIn, add: UGenIn )
extends SingleOutUGen( in, mul, add ) {
   override def toString = in.toString + ".madd(" + mul + ", " + add + ")"
}

abstract class BasicOpUGen( override val specialIndex: Int, inputs: UGenIn* )
extends SingleOutUGen( inputs: _* )

object UnaryOpUGen {
   sealed abstract class Op( val id: Int ) {
      def make( a: GE ) : GE = UnaryOpUGen.make( this, a )

      def name = { val cn = getClass.getName
         val sz   = cn.length
         val i    = cn.indexOf( '$' ) + 1
         cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
      }
   }
   case object Neg         extends Op(  0 )
   case object Not         extends Op(  1 )
// case object IsNil       extends Op(  2 )
// case object NotNil      extends Op(  3 )
// case object BitNot      extends Op(  4 )
   case object Abs         extends Op(  5 )
// case object ToFloat     extends Op(  6 )
// case object ToInt       extends Op(  7 )
   case object Ceil        extends Op(  8 )
   case object Floor       extends Op(  9 )
   case object Frac        extends Op( 10 )
   case object Signum      extends Op( 11 )
   case object Squared     extends Op( 12 )
   case object Cubed       extends Op( 13 )
   case object Sqrt        extends Op( 14 )
   case object Exp         extends Op( 15 )
   case object Reciprocal  extends Op( 16 )
   case object Midicps     extends Op( 17 )
   case object Cpsmidi     extends Op( 18 )
   case object Midiratio   extends Op( 19 )
   case object Ratiomidi   extends Op( 20 )
   case object Dbamp       extends Op( 21 )
   case object Ampdb       extends Op( 22 )
   case object Octcps      extends Op( 23 )
   case object Cpsoct      extends Op( 24 )
   case object Log         extends Op( 25 )
   case object Log2        extends Op( 26 )
   case object Log10       extends Op( 27 )
   case object Sin         extends Op( 28 )
   case object Cos         extends Op( 29 )
   case object Tan         extends Op( 30 )
   case object Asin        extends Op( 31 )
   case object Acos        extends Op( 32 )
   case object Atan        extends Op( 33 )
   case object Sinh        extends Op( 34 )
   case object Cosh        extends Op( 35 )
   case object Tanh        extends Op( 36 )
// class Rand              extends Op( 37 )
// class Rand2             extends Op( 38 )
// class Linrand           extends Op( 39 )
// class Bilinrand         extends Op( 40 )
// class Sum3rand          extends Op( 41 )
   case object Distort     extends Op( 42 )
   case object Softclip    extends Op( 43 )
// class Coin              extends Op( 44 )
// case object DigitValue  extends Op( 45 )
// case object Silence     extends Op( 46 )
// case object Thru        extends Op( 47 )
// case object RectWindow  extends Op( 48 )
// case object HanWindow   extends Op( 49 )
// case object WelWindow   extends Op( 50 )
// case object TriWindow   extends Op( 51 )
   case object Ramp        extends Op( 52 )
   case object Scurve      extends Op( 53 )

   protected[synth] def make( selector: Op, a: GE ) : GE = {
      simplify( for( List( ai ) <- expand( a )) yield make1( selector, ai ))
   }

//   @inline private def cn( f: Float )  = c( f )
//   @inline private def cn( d: Double ) = c( d.toFloat )

   private def make1( selector: Op, a: UGenIn ) : GE = {
      this( a.rate, selector, a )
//      // replace constants immediately
//      a match {
//         case c( aval ) => selector match {
//            case Neg       => cn( -aval )
//         // case Not       =>
//            case Abs       => cn( abs( aval ))
//            case Ceil      => cn( ceil( aval ))
//            case Floor     => cn( floor( aval ))
//            case Frac      => cn( aval - floor( aval )) // according to jmc
//            case Signum    => cn( signum( aval ))
//            case Squared   => cn( aval * aval )
//            case Cubed     => cn( aval * aval * aval )
//            case Sqrt      => cn( sqrt( aval ))
//            case Exp       => cn( exp( aval ))
//            case Reciprocal=> cn( 1.0f / aval )
//            case Midicps   => cn( 440 * pow( 2, (aval - 69) * 0.083333333333 ))
//            case Cpsmidi   => cn( log( aval * 0.0022727272727 ) / log( 2 ) * 12 + 69 )
//            case Midiratio => cn( pow( 2, aval * 0.083333333333 ))
//            case Ratiomidi => cn( 12 * log( aval ) / log( 2 ))
//            case Dbamp     => cn( pow( 10, aval * 0.05 ))
//            case Ampdb     => cn( log10( aval )* 20 )
//            case Octcps    => cn( 440 * pow( 2, aval - 4.75 ))
//            case Cpsoct    => cn( log( aval * 0.0022727272727 ) / log( 2 ) + 4.75 )
//            case Log       => cn( log( aval ))
//            case Log2      => cn( log( aval ) / log( 2 ))
//            case Log10     => cn( log10( aval ))
//            case Sin       => cn( sin( aval ))
//            case Cos       => cn( cos( aval ))
//            case Tan       => cn( tan( aval ))
//            case Asin      => cn( asin( aval ))
//            case Acos      => cn( acos( aval ))
//            case Atan      => cn( atan( aval ))
//            case Sinh      => cn( sinh( aval ))
//            case Cosh      => cn( cosh( aval ))
//            case Tanh      => cn( tanh( aval ))
//            case Distort   => cn( aval / (1 + abs( aval )))
//            case Softclip  =>	{ val absx = abs( aval ); cn( if( absx <= 0.5f ) aval else (absx - 0.25f) / aval )}
//            case Ramp      => cn( if( aval <= 0 ) 0 else if( aval >= 1 ) 1 else aval )
//            case Scurve    => cn( if( aval <= 0 ) 0 else if( aval > 1 ) 1 else aval * aval * (3 - 2 * aval))
//            case _         => this( a.rate, selector, a )
//         }
//         case _            => this( a.rate, selector, a )
//      }
   }
}

// Note: only deterministic selectors are implemented!!
case class UnaryOpUGen( rate: Rate, selector: UnaryOpUGen.Op, a: UGenIn )
extends BasicOpUGen( selector.id, a ) {
   override def toString = a.toString + "." + selector.name
}

object BinaryOpUGen {
   sealed abstract class Op( val id: Int ) {
      def make( a: GE, b: GE ) : GE = BinaryOpUGen.make( this, a, b )

      def name = { val cn = getClass.getName
         val sz   = cn.length
         val i    = cn.indexOf( '$' ) + 1
         cn.charAt( i ).toLower + cn.substring( i + 1, if( cn.charAt( sz - 1 ) == '$' ) sz - 1 else sz )
      }
   }
   case object Plus           extends Op(  0 ) { override val name = "+" }
   case object Minus          extends Op(  1 ) { override val name = "-" }
   case object Times          extends Op(  2 ) { override val name = "*" }
// case object IDiv           extends Op(  3 )
   case object Div            extends Op(  4 ) { override val name = "/" }
   case object Mod            extends Op(  5 ) { override val name = "%" }
   case object Eq             extends Op(  6 ) { override val name = "===" }
   case object Neq            extends Op(  7 ) { override val name = "!==" }
   case object Lt             extends Op(  8 ) { override val name = "<" }
   case object Gt             extends Op(  9 ) { override val name = ">" }
   case object Leq            extends Op( 10 ) { override val name = "<=" }
   case object Geq            extends Op( 11 ) { override val name = ">=" }
   case object Min            extends Op( 12 )
   case object Max            extends Op( 13 )
   case object BitAnd         extends Op( 14 ) { override val name = "&" }
   case object BitOr          extends Op( 15 ) { override val name = "|" }
   case object BitXor         extends Op( 16 ) { override val name = "^" }
// case object Lcm            extends Op( 17 )
// case object Gcd            extends Op( 18 )
   case object Round          extends Op( 19 )
   case object Roundup        extends Op( 20 )
   case object Trunc          extends Op( 21 )
   case object Atan2          extends Op( 22 )
   case object Hypot          extends Op( 23 )
   case object Hypotx         extends Op( 24 )
   case object Pow            extends Op( 25 )
// case object <<             extends Op( 26 )
// case object >>             extends Op( 27 )
// case object UnsgnRghtShft  extends Op( 28 )
// case object Fill           extends Op( 29 )
   case object Ring1          extends Op( 30 )
   case object Ring2          extends Op( 31 )
   case object Ring3          extends Op( 32 )
   case object Ring4          extends Op( 33 )
   case object Difsqr         extends Op( 34 )
   case object Sumsqr         extends Op( 35 )
   case object Sqrsum         extends Op( 36 )
   case object Sqrdif         extends Op( 37 )
   case object Absdif         extends Op( 38 )
   case object Thresh         extends Op( 39 )
   case object Amclip         extends Op( 40 )
   case object Scaleneg       extends Op( 41 )
   case object Clip2          extends Op( 42 )
   case object Excess         extends Op( 43 )
   case object Fold2          extends Op( 44 )
   case object Wrap2          extends Op( 45 )
   case object Firstarg       extends Op( 46 )
// case object Rrand          extends Op( 47 )
// case object ExpRRand       extends Op( 48 )

   protected[synth] def make( selector: Op, a: GE, b: GE ) : GE = {
      simplify( for( List( ai, bi ) <- expand( a, b )) yield make1( selector, ai, bi ))
   }

   @inline private def cn( f: Float )  = c( f )
   @inline private def cn( d: Double ) = c( d.toFloat )

   protected[synth] def make1( selector: Op, a: UGenIn, b: UGenIn ) : GE = {
      // XXX TODO: case (_, c(ac), c(bc)) ! 
      (selector, a, b) match {
         case (Times, c(0), _)   => a
         case (Times, _, c(0))   => b
         case (Times, c(1), _)   => b
         case (Times, _, c(1))   => a
         case (Times, c(-1), _)  => b.neg
         case (Times, _, c(-1))  => a.neg

         case (Plus, c(0), _)    => b
         case (Plus, _, c(0))    => a

         case (Minus, c(0), _)   => b.neg
         case (Minus, _, c(0))   => a

         case (Div, _, c(1))     => a
         case (Div, _, c(-1))    => a.neg
         case (Div, _, _) if b.rate == scalar => a * b.reciprocal

         case _ => this( Rates.highest( a.rate, b.rate ), selector, a, b )
      }
  }

/*
  private def determineRate( a: UGenIn, b: UGenIn ) : Rate = {
    if( a.rate > b.rate ) a.rate else b.rate
//    if( a.rate == 'demand ) return 'demand
//    if( b.rate == 'demand ) return 'demand
//    if( a.rate == 'audio ) return 'audio
//    if( b.rate == 'audio ) return 'audio
//    if( a.rate == 'control ) return 'control
//    if( b.rate == 'control ) return 'control
//    'scalar
  }
  */
}

// Note: only deterministic selectors are implemented!!
case class BinaryOpUGen( rate: Rate, selector: BinaryOpUGen.Op, a: UGenIn, b: UGenIn )
extends BasicOpUGen( selector.id, a, b ) {
   override def toString = if( (selector.id <= 11) || ((selector.id >=14) && (selector.id <= 16)) ) {
      "(" + a + " " + selector.name + " " + b + ")"
   } else {
      a.toString + "." + selector.name + "(" + b + ")"
   }
}