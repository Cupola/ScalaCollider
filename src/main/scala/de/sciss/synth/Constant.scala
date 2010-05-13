/*
 *  Constant.scala
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

/**
 *	@version	   0.13, 13-May-10
 */
object Constant {
   @inline private def cn( f: Float )     = apply( f )
   @inline private def cn( d: Double )    = apply( d.toFloat )
   @inline private def cn( b: Boolean )   = apply( if( b ) 1f else 0f )

   @inline private def fold( in: Float, lo: Float, hi: Float ) : Float = {
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

   @inline private def wrap( in: Float, lo: Float, hi: Float ) : Float = {
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
}

case class Constant( value: Float ) extends UGenIn with ScalarRated {
   import Constant._

   override def toString = value.toString
   
   // unary ops - refine GE to return constants,
   // since this way we can implicitly go back to Float
   override def neg : Constant         = cn( -value )
   override def abs : Constant	      = cn( math.abs( value ))
   override def ceil : Constant	      = cn( math.ceil( value ))
   override def floor : Constant	      = cn( math.floor( value ))
   override def frac : Constant	      = cn( value - math.floor( value )) // according to jmc
   override def signum : Constant      = cn( math.signum( value ))
   override def squared : Constant     = cn( value * value )
   override def cubed : Constant       = cn( value * value * value )
   override def sqrt : Constant        = cn( math.sqrt( value ))
   override def exp : Constant         = cn( math.exp( value ))
   override def reciprocal : Constant  = cn( 1.0f / value )
   override def midicps : Constant     = cn( 440 * math.pow( 2, (value - 69) * 0.083333333333 ))
   override def cpsmidi : Constant     = cn( math.log( value * 0.0022727272727 ) / math.log( 2 ) * 12 + 69 )
   override def midiratio : Constant   = cn( math.pow( 2, value * 0.083333333333 ))
   override def ratiomidi : Constant   = cn( 12 * math.log( value ) / math.log( 2 ))
   override def dbamp : Constant       = cn( math.pow( 10, value * 0.05 ))
   override def ampdb : Constant       = cn( math.log10( value )* 20 )
   override def octcps : Constant      = cn( 440 * math.pow( 2, value - 4.75 ))
   override def cpsoct : Constant      = cn( math.log( value * 0.0022727272727 ) / math.log( 2 ) + 4.75 )
   override def log : Constant         = cn( math.log( value ))
   override def log2 : Constant        = cn( math.log( value ) / math.log( 2 ))
   override def log10 : Constant       = cn( math.log10( value ))
   override def sin : Constant         = cn( math.sin( value ))
   override def cos : Constant         = cn( math.cos( value ))
   override def tan : Constant         = cn( math.tan( value ))
   override def asin : Constant        = cn( math.asin( value ))
   override def acos : Constant        = cn( math.acos( value ))
   override def atan : Constant        = cn( math.atan( value ))
   override def sinh : Constant        = cn( math.sinh( value ))
   override def cosh : Constant        = cn( math.cosh( value ))
   override def tanh : Constant        = cn( math.tanh( value ))
   override def distort : Constant     = cn( value / (1 + math.abs( value )))
   override def softclip : Constant    = { val absx = math.abs( value ); cn( if( absx <= 0.5f ) value else (absx - 0.25f) / value )}
   override def ramp : Constant        = cn( if( value <= 0 ) 0 else if( value >= 1 ) 1 else value )
   override def scurve : Constant      = cn( if( value <= 0 ) 0 else if( value > 1 ) 1 else value * value * (3 - 2 * value))

   // binary ops
   def +( b: Constant ) : Constant        = cn( value + b.value )
   def -( b: Constant ) : Constant        = cn( value - b.value )
   def *( b: Constant ) : Constant        = cn( value * b.value )
   def /( b: Constant ) : Constant        = cn( value / b.value )
   def %( b: Constant ) : Constant        = cn( value % b.value )
   def ===( b: Constant ) : Constant      = cn( value == b.value )
   def !==( b: Constant ) : Constant      = cn( value != b.value )
   def <( b: Constant ) : Constant	      = cn( value < b.value )
   def >( b: Constant ) : Constant	      = cn( value > b.value )
   def <=( b: Constant ) : Constant	      = cn( value <= b.value )
   def >=( b: Constant ) : Constant	      = cn( value >= b.value )
   def min( b: Constant ) : Constant      = cn( math.min( value, b.value ))
   def max( b: Constant ) : Constant      = cn( math.max( value, b.value ))
   def &( b: Constant ) : Constant	      = cn( value.toInt & b.value.toInt )
   def |( b: Constant ) : Constant	      = cn( value.toInt | b.value.toInt )
   def ^( b: Constant ) : Constant	      = cn( value.toInt ^ b.value.toInt )
   def round( b: Constant ) : Constant    = if( b.value == 0 ) this else cn( math.floor( value / b.value + 0.5f ) * b.value )
   def roundup( b: Constant ) : Constant  = if( b.value == 0 ) this else cn( math.ceil( value / b.value ) * b.value )
   def trunc( b: Constant ) : Constant    = if( b.value == 0 ) this else cn( math.floor( value / b.value ) * b.value )
   def atan2( b: Constant ) : Constant    = cn( math.atan2( value, b.value ))
   def hypot( b: Constant ) : Constant    = cn( math.hypot( value, b.value ))
   def hypotx( b: Constant ) : Constant   = {
      val minab = math.min( math.abs( value ), math.abs( b.value ))
      cn( value + b.value - (math.sqrt(2) - 1) * minab )
   }
   def pow( b: Constant ) : Constant      = cn( math.pow( value, b.value ))
   def ring1( b: Constant ) : Constant    = cn( value * b.value + value )
   def ring2( b: Constant ) : Constant    = cn( value * b.value + value + b.value )
   def ring3( b: Constant ) : Constant    = cn( value * value * b.value )
   def ring4( b: Constant ) : Constant    = { val ab = value * b.value; cn( value * ab - b.value * ab )}
   def difsqr( b: Constant ) : Constant   = cn( value * value - b.value * b.value )
   def sumsqr( b: Constant ) : Constant   = cn( value * value + b.value * b.value )
   def sqrsum( b: Constant ) : Constant   = { val z = value + b.value; cn( z * z )}
   def sqrdif( b: Constant ) : Constant   = { val z = value - b.value; cn( z * z )}
   def absdif( b: Constant ) : Constant   = cn( math.abs( value - b.value ))
   def thresh( b: Constant ) : Constant   = cn( if( value < b.value ) 0 else value )
   def amclip( b: Constant ) : Constant   = cn( value * 0.5f * (b.value + math.abs( value )))
   def scaleneg( b: Constant ) : Constant = cn( (math.abs( value ) - value) * (0.5f * b.value + 0.5f) + value )
   def clip2( b: Constant ) : Constant    = cn( math.max( math.min( value, b.value ), -b.value ))
   def excess( b: Constant ) : Constant   = cn( value - math.max( math.min( value, b.value ), -b.value ))
   def fold2( b: Constant ) : Constant    = cn( fold( value, -b.value, b.value ))
   def wrap2( b: Constant ) : Constant    = cn( wrap( value, -b.value, b.value ))
   def firstarg( b: Constant ) : Constant = this
}