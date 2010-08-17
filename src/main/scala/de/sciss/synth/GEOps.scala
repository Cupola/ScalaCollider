/*
 *  GEOps.scala
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

import ugen.{ BinaryOpUGen, UnaryOpUGen, LinLin, LinExp }

class GEOps private[synth]( a: GE ) {

   import UnaryOpUGen._

   // unary ops
   def unary_- : GE           = Neg.make( a )
// def bitNot : GE	         = BitNot.make( this )
   def abs : GE	            = Abs.make( a )
// def toFloat : GE	         = UnOp.make( 'asFloat, this )
// def toInteger : GE	      = UnOp.make( 'asInteger, this )
   def ceil : GE	            = Ceil.make( a )
   def floor : GE	            = Floor.make( a )
   def frac : GE	            = Frac.make( a )
   def signum : GE	         = Signum.make( a )
   def squared : GE           = Squared.make( a )
   def cubed : GE             = Cubed.make( a )
   def sqrt : GE              = Sqrt.make( a )
   def exp : GE               = Exp.make( a )
   def reciprocal : GE        = Reciprocal.make( a )
   def midicps : GE           = Midicps.make( a )
   def cpsmidi : GE           = Cpsmidi.make( a )
   def midiratio : GE         = Midiratio.make( a )
   def ratiomidi : GE         = Ratiomidi.make( a )
   def dbamp : GE             = Dbamp.make( a )
   def ampdb : GE             = Ampdb.make( a )
   def octcps : GE            = Octcps.make( a )
   def cpsoct : GE            = Cpsoct.make( a )
   def log : GE               = Log.make( a )
   def log2 : GE              = Log2.make( a )
   def log10 : GE             = Log10.make( a )
   def sin : GE               = Sin.make( a )
   def cos : GE               = Cos.make( a )
   def tan : GE               = Tan.make( a )
   def asin : GE              = Asin.make( a )
   def acos : GE              = Acos.make( a )
   def atan : GE              = Atan.make( a )
   def sinh : GE              = Sinh.make( a )
   def cosh : GE              = Cosh.make( a )
   def tanh : GE              = Tanh.make( a )
// def rand : GE              = UnOp.make( 'rand, this )
// def rand2 : GE             = UnOp.make( 'rand2, this )
// def linrand : GE           = UnOp.make( 'linrand, this )
// def bilinrand : GE         = UnOp.make( 'bilinrand, this )
// def sum3rand : GE          = UnOp.make( 'sum3rand, this )
   def distort : GE           = Distort.make( a )
   def softclip : GE          = Softclip.make( a )
// def coin : GE              = UnOp.make( 'coin, this )
// def even : GE              = UnOp.make( 'even, this )
// def odd : GE               = UnOp.make( 'odd, this )
// def rectWindow : GE        = UnOp.make( 'rectWindow, this )
// def hanWindow : GE         = UnOp.make( 'hanWindow, this )
// def welWindow : GE         = UnOp.make( 'sum3rand, this )
// def triWindow : GE         = UnOp.make( 'triWindow, this )
   def ramp : GE              = Ramp.make( a )
   def scurve : GE            = Scurve.make( a )
// def isPositive : GE        = UnOp.make( 'isPositive, this )
// def isNegative : GE        = UnOp.make( 'isNegative, this )
// def isStrictlyPositive : GE= UnOp.make( 'isStrictlyPositive, this )
// def rho : GE               = UnOp.make( 'rho, this )
// def theta : GE             = UnOp.make( 'theta, this )

   import BinaryOpUGen._

   // binary ops
  // NOTE: we need to put + directly in GE because
  // sucky scalac otherwise things it is as likely a
  // conversion as toString !!! and since + is
  // anyway defined for Float, Double, Int, there is
  // no need to lower its priority in GE
//   def +( b: GE ) : GE        = Plus.make( a, b )
   def -( b: GE ) : GE        = Minus.make( a, b )
   def *( b: GE ) : GE        = Times.make( a, b )
// def div( b: GE ) : GE      = IDiv.make( a, b )
   def /( b: GE ) : GE        = Div.make( a, b )
   def %( b: GE ) : GE        = Mod.make( a, b )
   def ===( b: GE ) : GE      = Eq.make( a, b )
   def !==( b: GE ) : GE      = Neq.make( a, b )
   def <( b: GE ) : GE	      = Lt.make( a, b )
   def >( b: GE ) : GE	      = Gt.make( a, b )
   def <=( b: GE ) : GE	      = Leq.make( a, b )
   def >=( b: GE ) : GE	      = Geq.make( a, b )
   def min( b: GE ) : GE      = Min.make( a, b )
   def max( b: GE ) : GE      = Max.make( a, b )
   def &( b: GE ) : GE	      = BitAnd.make( a, b )
   def |( b: GE ) : GE	      = BitOr.make( a, b )
   def ^( b: GE ) : GE	      = BitXor.make( a, b )
// def Lcm( b: GE ) : GE      = Lcm.make( a, b )
// def Gcd( b: GE ) : GE      = Gcd.make( a, b )
   def round( b: GE ) : GE    = Round.make( a, b )
   def roundup( b: GE ) : GE  = Roundup.make( a, b ) // sclang uses camel case instead
   def trunc( b: GE ) : GE    = Trunc.make( a, b )
   def atan2( b: GE ) : GE    = Atan2.make( a, b )
   def hypot( b: GE ) : GE    = Hypot.make( a, b )
   def hypotx( b: GE ) : GE   = Hypotx.make( a, b )
   def pow( b: GE ) : GE      = Pow.make( a, b )
// def <<( b: GE ) : GE       = <<.make( a, b )
// def >>( b: GE ) : GE       = >>.make( a, b )
// def unsgnRghtShift( b: GE ) : GE = UnsgnRghtShift.make( a, b )
// def fill( b: GE ) : GE     = Fill.make( a, b )
   def ring1( b: GE ) : GE    = Ring1.make( a, b )
   def ring2( b: GE ) : GE    = Ring2.make( a, b )
   def ring3( b: GE ) : GE    = Ring3.make( a, b )
   def ring4( b: GE ) : GE    = Ring4.make( a, b )
   def difsqr( b: GE ) : GE   = Difsqr.make( a, b )
   def sumsqr( b: GE ) : GE   = Sumsqr.make( a, b )
   def sqrsum( b: GE ) : GE   = Sqrsum.make( a, b )
   def sqrdif( b: GE ) : GE   = Sqrdif.make( a, b )
   def absdif( b: GE ) : GE   = Absdif.make( a, b )
   def thresh( b: GE ) : GE   = Thresh.make( a, b )
   def amclip( b: GE ) : GE   = Amclip.make( a, b )
   def scaleneg( b: GE ) : GE = Scaleneg.make( a, b )
   def clip2( b: GE ) : GE    = Clip2.make( a, b )
   def excess( b: GE ) : GE   = Excess.make( a, b )
   def fold2( b: GE ) : GE    = Fold2.make( a, b )
   def wrap2( b: GE ) : GE    = Wrap2.make( a, b )
   def firstarg( b: GE ) : GE = Firstarg.make( a, b ) // sclang uses camel case instead
// def rrand( b: GE ) : GE    = Rrand.make( a, b )
// def exprrand( b: GE ) : GE = Exprrand.make( a, b )

   // other ugens
//   def linlin( srcLo: GE, srcHi: GE, dstLo: GE, dstHi: GE ) : GE = {
//      val rate = Rate.highest( a.outputs.map( _.rate ))
//      simplify( for( List( ax, sl, sh, dl, dh ) <- expand( a, srcLo, srcHi, dstLo, dstHi ))
//         yield LinLin( rate, ax, sl, sh, dl, dh ))
//   }
   def linlin( srcLo: GE, srcHi: GE, dstLo: GE, dstHi: GE ) : GE = Rate.highest( a ) match {
      case `demand` => (a - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo
      case r => LinLin.make( r, a, srcLo, srcHi, dstLo, dstHi ) // should be highest rate of all inputs? XXX
   }

   def linexp( srcLo: GE, srcHi: GE, dstLo: GE, dstHi: GE ) : GE = Rate.highest( a ) match {
      case `demand` => (dstHi / dstLo).pow( (a - srcLo) / (srcHi - srcLo) ) * dstLo
      case r => LinExp.make( r, a, srcLo, srcHi, dstLo, dstHi ) // should be highest rate of all inputs? XXX
   }
}