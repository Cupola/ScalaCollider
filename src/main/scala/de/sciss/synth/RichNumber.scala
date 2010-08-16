/*
 *  RichNumber.scala
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

// ---------------------------- Float ----------------------------

/**
 * @version 0.11, 16-Aug-10
 */
object RichFloat {
   @inline private[synth] def rf_fold( in: Float, lo: Float, hi: Float ) : Float = {
      val x = in - lo
      // avoid the divide if possible
      if( in >= hi ) {
         val f = hi + hi - in
         if (f >= lo) return f
      } else if( in < lo ) {
         val f = lo + lo - in
         if( f < hi ) return f
      } else return in

      if( hi == lo ) return lo
      // ok do the divide
      val range   = hi - lo
      val range2  = range + range
      val c       = x - range2 * math.floor( x / range2 ).toFloat
      lo + (if( c >= range ) range2 - c else c)
   }

   @inline private[synth] def rf_wrap( in: Float, lo: Float, hi: Float ) : Float = {
      // avoid the divide if possible
      if( in >= hi ) {
         val range   = hi - lo
         val in2     = in - range;
         if( in2 < hi ) in2 else if( hi == lo ) lo else {
            in2 - range * math.floor( (in2 - lo) / range ).toFloat
         }
      } else if( in < lo ) {
         val range   = hi - lo
         val in2     = in + range
         if( in2 >= lo ) in2 else if( hi == lo ) lo else {
            in2 - range * math.floor( (in2 - lo) / range ).toFloat
         }
      } else in
   }

   @inline private[synth] def rf_round( a: Float, b: Float ) =
      if( b == 0 ) a else (math.floor( a / b + 0.5f ) * b).toFloat

   @inline private[synth] def rf_roundup( a: Float, b: Float ) =
      if( b == 0 ) a else (math.ceil( a / b ) * b).toFloat

   @inline private[synth] def rf_trunc( a: Float, b: Float ) =
      if( b == 0 ) a else (math.floor( a / b ) * b).toFloat

   @inline private[synth] def rf_hypotx( a: Float, b: Float ) = {
      val minab = math.min( math.abs( a ), math.abs( b ))
      (a + b - (math.sqrt(2) - 1) * minab).toFloat
   }

   @inline private[synth] def rf_ring1( a: Float, b: Float ) =
      a * b + a

   @inline private[synth] def rf_ring2( a: Float, b: Float ) =
      a * b + a + b

   @inline private[synth] def rf_ring3( a: Float, b: Float ) =
      a * a * b

   @inline private[synth] def rf_ring4( a: Float, b: Float ) = {
      val ab = a * b; a * ab - b * ab
   }

   @inline private[synth] def rf_difsqr( a: Float, b: Float ) =
      a * a - b * b

   @inline private[synth] def rf_sumsqr( a: Float, b: Float ) =
      a * a + b * b

   @inline private[synth] def rf_sqrsum( a: Float, b: Float ) = {
      val z = a + b; z * z
   }

   @inline private[synth] def rf_sqrdif( a: Float, b: Float ) = {
      val z = a - b; z * z
   }

   @inline private[synth] def rf_absdif( a: Float, b: Float ) = math.abs( a - b )

   @inline private[synth] def rf_thresh( a: Float, b: Float ) =
      if( a < b ) 0 else a

   @inline private[synth] def rf_amclip( a: Float, b: Float ) =
      a * 0.5f * (b + math.abs( a ))

   @inline private[synth] def rf_scaleneg( a: Float, b: Float ) =
      (math.abs( a ) - a) * (0.5f * b + 0.5f) + a

   @inline private[synth] def rf_clip2( a: Float, b: Float ) =
      math.max( math.min( a, b ), -b )

   @inline private[synth] def rf_excess( a: Float, b: Float ) =
      a - math.max( math.min( a, b ), -b )

   @inline private[synth] def rf_fold2( a: Float, b: Float ) = rf_fold( a, -b, b )

   @inline private[synth] def rf_wrap2( a: Float, b: Float ) = rf_wrap( a, -b, b )

   @inline private[synth] def rf_linlin( in: Float, srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float ) : Float = {
      (in - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo
   }

   @inline private def rf_linexp( in: Float, srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float ) : Float = {
      math.pow( dstHi / dstLo, (in - srcLo) / (srcHi - srcLo)).toFloat * dstLo
   }
}

final case class RichFloat private[synth]( f: Float ) {
   import RichFloat._

   override def toString = f.toString

   @inline private def cn = Constant( f )

   // unary ops - refine GE to return constants,
   // since this way we can implicitly go back to Float
// def unary_- : Float     = -f
// def abs : Float	      = math.abs( f )
// def ceil : Float	      = math.ceil( f ).toFloat
// def floor : Float	      = math.floor( f ).toFloat
   def frac : Float	      = (f - math.floor( f )).toFloat // according to jmc
   def signum : Float      = math.signum( f )
   def squared : Float     = f * f
   def cubed : Float       = f * f * f
   def sqrt : Float        = math.sqrt( f ).toFloat
   def exp : Float         = math.exp( f ).toFloat
   def reciprocal : Float  = 1.0f / f
   def midicps : Float     = (440 * math.pow( 2, (f - 69) * 0.083333333333 )).toFloat
   def cpsmidi : Float     = (math.log( f * 0.0022727272727 ) / math.log( 2 ) * 12 + 69).toFloat
   def midiratio : Float   = (math.pow( 2, f * 0.083333333333 )).toFloat
   def ratiomidi : Float   = (12 * math.log( f ) / math.log( 2 )).toFloat
   def dbamp : Float       = (math.pow( 10, f * 0.05 )).toFloat
   def ampdb : Float       = (math.log10( f )* 20).toFloat
   def octcps : Float      = (440 * math.pow( 2, f - 4.75 )).toFloat
   def cpsoct : Float      = (math.log( f * 0.0022727272727 ) / math.log( 2 ) + 4.75).toFloat
   def log : Float         = math.log( f ).toFloat
   def log2 : Float        = (math.log( f ) / math.log( 2 )).toFloat
   def log10 : Float       = math.log10( f ).toFloat
   def sin : Float         = math.sin( f ).toFloat
   def cos : Float         = math.cos( f ).toFloat
   def tan : Float         = math.tan( f ).toFloat
   def asin : Float        = math.asin( f ).toFloat
   def acos : Float        = math.acos( f ).toFloat
   def atan : Float        = math.atan( f ).toFloat
   def sinh : Float        = math.sinh( f ).toFloat
   def cosh : Float        = math.cosh( f ).toFloat
   def tanh : Float        = math.tanh( f ).toFloat
// def distort : Float     = f / (1 + math.abs( f ))
// def softclip : Float    = { val absx = math.abs( f ); if( absx <= 0.5f ) f else (absx - 0.25f) / f}
// def ramp : Float        = if( f <= 0 ) 0 else if( f >= 1 ) 1 else f
// def scurve : Float      = if( f <= 0 ) 0 else if( f > 1 ) 1 else f * f * (3 - 2 * f)

   // binary ops
   // note: min, max, <, >, <=, >= are defined in scala.runtime.RichFloat,
   // therefore we do not define them again on Float as that would produce
   // an ambiguity!
// def +( b: Float ) : Float        = f + b.f
// def -( b: Float ) : Float        = f - b.f
   def -( b: GE ) : GE              = cn.-( b )
// def *( b: Float ) : Float        = f * b.f
   def *( b: GE ) : GE              = cn.*( b )
// def /( b: Float ) : Float        = f / b.f
   def /( b: GE ) : GE              = cn./( b )
// def %( b: Float ) : Float        = f % b.f
   def %( b: GE ) : GE              = cn.%( b )
// def ===( b: Float ) : Int        = if( f == b ) 1 else 0
   def ===( b: GE ) : GE            = cn.===( b )
// def !==( b: Float ) : Int        = if( f != b ) 1 else 0
   def !==( b: GE ) : GE            = cn.!==( b )
// def <( b: Float ) : Float	      = f < b.f
   def <( b: GE ) : GE              = cn.<( b )
// def >( b: Float ) : Float	      = f > b.f
   def >( b: GE ) : GE              = cn.>( b )
// def <=( b: Float ) : Float	      = f <= b.f
   def <=( b: GE ) : GE             = cn.<=( b )
// def >=( b: Float ) : Float	      = f >= b.f
   def >=( b: GE ) : GE             = cn.>=( b )
// def min( b: Float ) : Float      = math.min( f, b ).toFloat
   def min( b: GE ) : GE            = cn.min( b )
// def max( b: Float ) : Float      = math.max( f, b ).toFloat
   def max( b: GE ) : GE            = cn.max( b )
// def &( b: Float ) : Float	      = f.toInt & b.f.toInt
   def &( b: GE ) : GE              = cn.&( b )
// def |( b: Float ) : Float	      = f.toInt | b.f.toInt
   def |( b: GE ) : GE              = cn.|( b )
// def ^( b: Float ) : Float	      = f.toInt ^ b.f.toInt
   def ^( b: GE ) : GE              = cn.^( b )
   def round( b: Float ) : Float    = rf_round( f, b )
   def round( b: GE ) : GE          = cn.round( b )
   def roundup( b: Float ) : Float  = rf_roundup( f, b )
   def roundup( b: GE ) : GE        = cn.roundup( b )
   def trunc( b: Float ) : Float    = rf_trunc( f, b )
   def trunc( b: GE ) : GE          = cn.trunc( b )
   def atan2( b: Float ) : Float    = math.atan2( f, b ).toFloat
   def atan2( b: GE ) : GE          = cn.atan2( b )
   def hypot( b: Float ) : Float    = math.hypot( f, b ).toFloat
   def hypot( b: GE ) : GE          = cn.hypot( b )
   def hypotx( b: Float ) : Float   = rf_hypotx( f, b )
   def hypotx( b: GE ) : GE         = cn.hypotx( b )
   def pow( b: Float ) : Float      = math.pow( f, b ).toFloat
   def pow( b: GE ) : GE            = cn.pow( b )
   def ring1( b: Float ) : Float    = rf_ring1( f, b )
   def ring1( b: GE ) : GE          = cn.ring1( b )
   def ring2( b: Float ) : Float    = rf_ring2( f, b )
   def ring2( b: GE ) : GE          = cn.ring2( b )
   def ring3( b: Float ) : Float    = rf_ring3( f, b )
   def ring3( b: GE ) : GE          = cn.ring3( b )
   def ring4( b: Float ) : Float    = rf_ring4( f, b )
   def ring4( b: GE ) : GE          = cn.ring4( b )
   def difsqr( b: Float ) : Float   = rf_difsqr( f, b )
   def difsqr( b: GE ) : GE         = cn.difsqr( b )
   def sumsqr( b: Float ) : Float   = rf_sumsqr( f, b )
   def sumsqr( b: GE ) : GE         = cn.sumsqr( b )
   def sqrsum( b: Float ) : Float   = rf_sqrsum( f, b )
   def sqrsum( b: GE ) : GE         = cn.sqrsum( b )
   def sqrdif( b: Float ) : Float   = rf_sqrdif( f, b )
   def sqrdif( b: GE ) : GE         = cn.sqrdif( b )
   def absdif( b: Float ) : Float   = rf_absdif( f, b )
   def absdif( b: GE ) : GE         = cn.absdif( b )
   def thresh( b: Float ) : Float   = rf_thresh( f, b )
   def thresh( b: GE ) : GE         = cn.thresh( b )
   def amclip( b: Float ) : Float   = rf_amclip( f, b )
   def amclip( b: GE ) : GE         = cn.amclip( b )
   def scaleneg( b: Float ) : Float = rf_scaleneg( f, b )
   def scaleneg( b: GE ) : GE       = cn.scaleneg( b )
   def clip2( b: Float ) : Float    = rf_clip2( f, b )
   def clip2( b: GE ) : GE          = cn.clip2( b )
   def excess( b: Float ) : Float   = rf_excess( f, b )
   def excess( b: GE ) : GE         = cn.excess( b )
   def fold2( b: Float ) : Float    = rf_fold2( f, b )
   def fold2( b: GE ) : GE          = cn.fold2( b )
   def wrap2( b: Float ) : Float    = rf_wrap2( f, b )
   def wrap2( b: GE ) : GE          = cn.wrap2( b )
// def firstarg( b: Float ) : Float = d
   def firstarg( b: GE ) : GE       = cn.firstarg( b )

   def linlin( srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float ) : Float =
      rf_linlin( f, srcLo, srcHi, dstLo, dstHi )

   def linlin( srcLo: GE, srcHi: GE, dstLo: GE, dstHi: GE ) : GE =
      cn.linlin( srcLo, srcHi, dstLo, dstHi )

   def linexp( srcLo: Float, srcHi: Float, dstLo: Float, dstHi: Float ) : Float =
      rf_linexp( f, srcLo, srcHi, dstLo, dstHi )

   def linexp( srcLo: GE, srcHi: GE, dstLo: GE, dstHi: GE ) : GE =
      cn.linexp( srcLo, srcHi, dstLo, dstHi )
}

// ---------------------------- Double ----------------------------

object RichDouble {
   @inline private def rd_fold( in: Double, lo: Double, hi: Double ) : Double = {
      val x = in - lo
      // avoid the divide if possible
      if( in >= hi ) {
         val f = hi + hi - in
         if (f >= lo) return f
      } else if( in < lo ) {
         val f = lo + lo - in
         if( f < hi ) return f
      } else return in

      if( hi == lo ) return lo
      // ok do the divide
      val range   = hi - lo
      val range2  = range + range
      val c       = x - range2 * math.floor( x / range2 )
      lo + (if( c >= range ) range2 - c else c)
   }

   @inline private def rd_wrap( in: Double, lo: Double, hi: Double ) : Double = {
      // avoid the divide if possible
      if( in >= hi ) {
         val range   = hi - lo
         val in2     = in - range;
         if( in2 < hi ) in2 else if( hi == lo ) lo else {
            in2 - range * math.floor( (in2 - lo) / range )
         }
      } else if( in < lo ) {
         val range   = hi - lo
         val in2     = in + range
         if( in2 >= lo ) in2 else if( hi == lo ) lo else {
            in2 - range * math.floor( (in2 - lo) / range )
         }
      } else in
   }

   @inline private def rd_linlin( in: Double, srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double = {
      (in - srcLo) / (srcHi - srcLo) * (dstHi - dstLo) + dstLo
   }

   @inline private def rd_linexp( in: Double, srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double = {
      math.pow( dstHi / dstLo, (in - srcLo) / (srcHi - srcLo)) * dstLo
   }
}

final case class RichDouble private[synth]( d: Double ) {
   import RichDouble._

   override def toString = d.toString

   @inline private def cn = Constant( d.toFloat )

   // unary ops - refine GE to return constants,
   // since this way we can implicitly go back to Double
//   def unary_- : Double    = -d
//   def abs : Double	      = math.abs( d )
//   def ceil : Double	      = math.ceil( d )
//   def floor : Double	   = math.floor( d )
   def frac : Double	      = (d - math.floor( d )) // according to jmc
   def signum : Double     = math.signum( d )
   def squared : Double    = d * d
   def cubed : Double      = d * d * d
   def sqrt : Double       = math.sqrt( d )
   def exp : Double        = math.exp( d )
   def reciprocal : Double = 1.0 / d
   def midicps : Double    = (440 * math.pow( 2, (d - 69) * 0.083333333333 ))
   def cpsmidi : Double    = (math.log( d * 0.0022727272727 ) / math.log( 2 ) * 12 + 69)
   def midiratio : Double  = (math.pow( 2, d * 0.083333333333 ))
   def ratiomidi : Double  = (12 * math.log( d ) / math.log( 2 ))
   def dbamp : Double      = (math.pow( 10, d * 0.05 ))
   def ampdb : Double      = (math.log10( d )* 20)
   def octcps : Double     = (440 * math.pow( 2, d - 4.75 ))
   def cpsoct : Double     = (math.log( d * 0.0022727272727 ) / math.log( 2 ) + 4.75)
   def log : Double        = math.log( d )
   def log2 : Double       = (math.log( d ) / math.log( 2 ))
   def log10 : Double      = math.log10( d )
   def sin : Double        = math.sin( d )
   def cos : Double        = math.cos( d )
   def tan : Double        = math.tan( d )
   def asin : Double       = math.asin( d )
   def acos : Double       = math.acos( d )
   def atan : Double       = math.atan( d )
   def sinh : Double       = math.sinh( d )
   def cosh : Double       = math.cosh( d )
   def tanh : Double       = math.tanh( d )
//   def distort : Double    = d / (1 + math.abs( d ))
//   def softclip : Double   = { val absx = math.abs( d ); if( absx <= 0.5 ) d else (absx - 0.25) / d}
//   def ramp : Double       = if( d <= 0 ) 0 else if( d >= 1 ) 1 else d
//   def scurve : Double     = if( d <= 0 ) 0 else if( d > 1 ) 1 else d * d * (3 - 2 * d)

   // binary ops
   def -( b: GE ) : GE                 = cn.-( b )
   def *( b: GE ) : GE                 = cn.*( b )
   def /( b: GE ) : GE                 = cn./( b )
   def %( b: GE ) : GE                 = cn.%( b )
   def ===( b: GE ) : GE               = cn.===( b )
   def !==( b: GE ) : GE               = cn.!==( b )
   def <( b: GE ) : GE                 = cn.<( b )
   def >( b: GE ) : GE                 = cn.>( b )
   def <=( b: GE ) : GE                = cn.<=( b )
   def >=( b: GE ) : GE                = cn.>=( b )
   def min( b: GE ) : GE               = cn.min( b )
   def max( b: GE ) : GE               = cn.max( b )
   def &( b: GE ) : GE                 = cn.&( b )
   def |( b: GE ) : GE                 = cn.|( b )
   def ^( b: GE ) : GE                 = cn.^( b )
   def round( b: Double ) : Double     = if( b == 0 ) d else (math.floor( d / b + 0.5 ) * b)
   def round( b: GE ) : GE             = cn.round( b )
   def roundup( b: Double ) : Double   = if( b == 0 ) d else (math.ceil( d / b ) * b)
   def roundup( b: GE ) : GE           = cn.roundup( b )
   def trunc( b: Double ) : Double     = if( b == 0 ) d else (math.floor( d / b ) * b)
   def trunc( b: GE ) : GE             = cn.trunc( b )
   def atan2( b: Double ) : Double     = math.atan2( d, b )
   def atan2( b: GE ) : GE             = cn.atan2( b )
   def hypot( b: Double ) : Double     = math.hypot( d, b )
   def hypot( b: GE ) : GE             = cn.hypot( b )
   def hypotx( b: Double ) : Double    = {
      val minab = math.min( math.abs( d ), math.abs( b ))
      (d + b - (math.sqrt(2) - 1) * minab)
   }
   def hypotx( b: GE ) : GE            = cn.hypotx( b )
   def pow( b: Double ) : Double       = math.pow( d, b )
   def pow( b: GE ) : GE               = cn.pow( b )
   def ring1( b: Double ) : Double     = d * b + d
   def ring1( b: GE ) : GE             = cn.ring1( b )
   def ring2( b: Double ) : Double     = d * b + d + b
   def ring2( b: GE ) : GE             = cn.ring2( b )
   def ring3( b: Double ) : Double     = d * d * b
   def ring3( b: GE ) : GE             = cn.ring3( b )
   def ring4( b: Double ) : Double     = { val ab = d * b; d * ab - b * ab }
   def ring4( b: GE ) : GE             = cn.ring4( b )
   def difsqr( b: Double ) : Double    = d * d - b * b
   def difsqr( b: GE ) : GE            = cn.difsqr( b )
   def sumsqr( b: Double ) : Double    = d * d + b * b
   def sumsqr( b: GE ) : GE            = cn.sumsqr( b )
   def sqrsum( b: Double ) : Double    = { val z = d + b; z * z }
   def sqrsum( b: GE ) : GE            = cn.sqrsum( b )
   def sqrdif( b: Double ) : Double    = { val z = d - b; z * z }
   def sqrdif( b: GE ) : GE            = cn.sqrdif( b )
   def absdif( b: Double ) : Double    = math.abs( d - b )
   def absdif( b: GE ) : GE            = cn.absdif( b )
   def thresh( b: Double ) : Double    = if( d < b ) 0 else d
   def thresh( b: GE ) : GE            = cn.thresh( b )
   def amclip( b: Double ) : Double    = d * 0.5 * (b + math.abs( d ))
   def amclip( b: GE ) : GE            = cn.amclip( b )
   def scaleneg( b: Double ) : Double  = (math.abs( d ) - d) * (0.5 * b + 0.5) + d
   def scaleneg( b: GE ) : GE          = cn.scaleneg( b )
   def clip2( b: Double ) : Double     = math.max( math.min( d, b ), -b )
   def clip2( b: GE ) : GE             = cn.clip2( b )
   def excess( b: Double ) : Double    = d - math.max( math.min( d, b ), -b )
   def excess( b: GE ) : GE            = cn.excess( b )
   def fold2( b: Double ) : Double     = rd_fold( d, -b, b )
   def fold2( b: GE ) : GE             = cn.fold2( b )
   def wrap2( b: Double ) : Double     = rd_wrap( d, -b, b )
   def wrap2( b: GE ) : GE             = cn.wrap2( b )
// def firstarg( b: Double ) : Double  = d
   def firstarg( b: GE ) : GE          = cn.firstarg( b )

   def linlin( srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double =
      rd_linlin( d, srcLo, srcHi, dstLo, dstHi )

   def linlin( srcLo: GE, srcHi: GE, dstLo: GE, dstHi: GE ) : GE =
      cn.linlin( srcLo, srcHi, dstLo, dstHi )

   def linexp( srcLo: Double, srcHi: Double, dstLo: Double, dstHi: Double ) : Double =
      rd_linexp( d, srcLo, srcHi, dstLo, dstHi )

   def linexp( srcLo: GE, srcHi: GE, dstLo: GE, dstHi: GE ) : GE =
      cn.linexp( srcLo, srcHi, dstLo, dstHi )
}