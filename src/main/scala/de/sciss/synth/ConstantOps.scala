package de.sciss.synth

import de.sciss.synth.{ Constant => C }

object ConstantOps {
   @inline private def cn( f: Float )     = Constant( f )
   @inline private def cn( d: Double )    = Constant( d.toFloat )
   @inline private def cn( b: Boolean )   = Constant( if( b ) 1f else 0f )

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

final private[synth] class ConstantOps( a: C ) extends GEOps( a ) {
   import ConstantOps._
   
   // unary ops. note: there is no sense in changing the
   // result type to Constant, as the class seen from outside
   // is always GEOps.
   override def unary_- : GE     = cn( -a.value )
   override def abs : GE	      = cn( math.abs( a.value ))
   override def ceil : GE	      = cn( math.ceil( a.value ))
   override def floor : GE	      = cn( math.floor( a.value ))
   override def frac : GE	      = cn( a.value - math.floor( a.value )) // according to jmc
   override def signum : GE      = cn( math.signum( a.value ))
   override def squared : GE     = cn( a.value * a.value )
   override def cubed : GE       = cn( a.value * a.value * a.value )
   override def sqrt : GE        = cn( math.sqrt( a.value ))
   override def exp : GE         = cn( math.exp( a.value ))
   override def reciprocal : GE  = cn( 1.0f / a.value )
   override def midicps : GE     = cn( 440 * math.pow( 2, (a.value - 69) * 0.083333333333 ))
   override def cpsmidi : GE     = cn( math.log( a.value * 0.0022727272727 ) / math.log( 2 ) * 12 + 69 )
   override def midiratio : GE   = cn( math.pow( 2, a.value * 0.083333333333 ))
   override def ratiomidi : GE   = cn( 12 * math.log( a.value ) / math.log( 2 ))
   override def dbamp : GE       = cn( math.pow( 10, a.value * 0.05 ))
   override def ampdb : GE       = cn( math.log10( a.value )* 20 )
   override def octcps : GE      = cn( 440 * math.pow( 2, a.value - 4.75 ))
   override def cpsoct : GE      = cn( math.log( a.value * 0.0022727272727 ) / math.log( 2 ) + 4.75 )
   override def log : GE         = cn( math.log( a.value ))
   override def log2 : GE        = cn( math.log( a.value ) / math.log( 2 ))
   override def log10 : GE       = cn( math.log10( a.value ))
   override def sin : GE         = cn( math.sin( a.value ))
   override def cos : GE         = cn( math.cos( a.value ))
   override def tan : GE         = cn( math.tan( a.value ))
   override def asin : GE        = cn( math.asin( a.value ))
   override def acos : GE        = cn( math.acos( a.value ))
   override def atan : GE        = cn( math.atan( a.value ))
   override def sinh : GE        = cn( math.sinh( a.value ))
   override def cosh : GE        = cn( math.cosh( a.value ))
   override def tanh : GE        = cn( math.tanh( a.value ))
   override def distort : GE     = cn( a.value / (1 + math.abs( a.value )))
   override def softclip : GE    = { val absx = math.abs( a.value ); cn( if( absx <= 0.5f ) a.value else (absx - 0.25f) / a.value )}
   override def ramp : GE        = cn( if( a.value <= 0 ) 0 else if( a.value >= 1 ) 1 else a.value )
   override def scurve : GE      = cn( if( a.value <= 0 ) 0 else if( a.value > 1 ) 1 else a.value * a.value * (3 - 2 * a.value))

//   // binary ops. we use pattern matching here...
//   // note that in real life, we will deal with
//   // Float literals, so operations on two Floats
//   // happen in RichFloat, not ConstantOps!
////   override def +( b: GE ) : GE        = b match {
////      case C( bval ) => cn( a.value + bval )
////      case _ => super.+( b )
////   }
//   override def -( b: GE ) : GE        = b match {
//      case C( bval ) => cn( a.value - bval )
//      case _ => super.-( b )
//   }
//   override def *( b: GE ) : GE        = b match {
//      case C( bval ) => cn( a.value * bval )
//      case _ => super.*( b )
//   }
//   override def /( b: GE ) : GE        = b match {
//      case C( bval ) => cn( a.value / bval )
//      case _ => super./( b )
//   }
//   override def %( b: GE ) : GE        = b match {
//      case C( bval ) => cn( a.value % bval )
//      case _ => super.%( b )
//   }
//   override def ===( b: GE ) : GE      = b match {
//      case C( bval ) => cn( a.value == bval )
//      case _ => super.===( b )
//   }
//   override def !==( b: GE ) : GE      = b match {
//      case C( bval ) => cn( a.value != bval )
//      case _ => super.!==( b )
//   }
//   override def <( b: GE ) : GE	      = b match {
//      case C( bval ) => cn( a.value < bval )
//      case _ => super.<( b )
//   }
//   override def >( b: GE ) : GE	      = b match {
//      case C( bval ) => cn( a.value > bval )
//      case _ => super.>( b )
//   }
//   override def <=( b: GE ) : GE	      = b match {
//      case C( bval ) => cn( a.value <= bval )
//      case _ => super.<=( b )
//   }
//   override def >=( b: GE ) : GE	      = b match {
//      case C( bval ) => cn( a.value >= bval )
//      case _ => super.>=( b )
//   }
//   override def min( b: GE ) : GE      = b match {
//      case C( bval ) => cn( math.min( a.value, bval ))
//      case _ => super.min( b )
//   }
//   override def max( b: GE ) : GE      = b match {
//      case C( bval ) => cn( math.max( a.value, bval ))
//      case _ => super.max( b )
//   }
//   override def &( b: GE ) : GE	      = b match {
//      case C( bval ) => cn( a.value.toInt & bval.toInt )
//      case _ => super.&( b )
//   }
//   override def |( b: GE ) : GE	      = b match {
//      case C( bval ) => cn( a.value.toInt | bval.toInt )
//      case _ => super.|( b )
//   }
//   override def ^( b: GE ) : GE	      = b match {
//      case C( bval ) => cn( a.value.toInt ^ bval.toInt )
//      case _ => super.^( b )
//   }
//   override def round( b: GE ) : GE    = b match {
//      case C( bval ) => if( bval == 0 ) a else cn( math.floor( a.value / bval + 0.5f ) * bval )
//      case _ => super.round( b )
//   }
//   override def roundup( b: GE ) : GE  = b match {
//      case C( bval ) => if( bval == 0 ) a else cn( math.ceil( a.value / bval ) * bval )
//      case _ => super.roundup( b )
//   }
//   override def trunc( b: GE ) : GE    = b match {
//      case C( bval ) => if( bval == 0 ) a else cn( math.floor( a.value / bval ) * bval )
//      case _ => super.trunc( b )
//   }
//   override def atan2( b: GE ) : GE    = b match {
//      case C( bval ) => cn( math.atan2( a.value, bval ))
//      case _ => super.atan2( b )
//   }
//   override def hypot( b: GE ) : GE    = b match {
//      case C( bval ) => cn( math.hypot( a.value, bval ))
//      case _ => super.hypot( b )
//   }
//   override def hypotx( b: GE ) : GE   = b match {
//      case C( bval ) => {
//         val minab = math.min( math.abs( a.value ), math.abs( bval ))
//         cn( a.value + bval - (math.sqrt(2) - 1) * minab )
//      }
//      case _ => super.hypotx( b )
//   }
//   override def pow( b: GE ) : GE      = b match {
//      case C( bval ) => cn( math.pow( a.value, bval ))
//      case _ => super.pow( b )
//   }
//   override def ring1( b: GE ) : GE    = b match {
//      case C( bval ) => cn( a.value * bval + a.value )
//      case _ => super.ring1( b )
//   }
//   override def ring2( b: GE ) : GE    = b match {
//      case C( bval ) => cn( a.value * bval + a.value + bval )
//      case _ => super.ring2( b )
//   }
//   override def ring3( b: GE ) : GE    = b match {
//      case C( bval ) => cn( a.value * a.value * bval )
//      case _ => super.ring3( b )
//   }
//   override def ring4( b: GE ) : GE    = b match {
//      case C( bval ) => {
//         val ab = a.value * bval
//         cn( a.value * ab - bval * ab )
//      }
//      case _ => super.ring4( b )
//   }
//   override def difsqr( b: GE ) : GE   = b match {
//      case C( bval ) => cn( a.value * a.value - bval * bval )
//      case _ => super.difsqr( b )
//   }
//   override def sumsqr( b: GE ) : GE   = b match {
//      case C( bval ) => cn( a.value * a.value + bval * bval )
//      case _ => super.sumsqr( b )
//   }
//   override def sqrsum( b: GE ) : GE   = b match {
//      case C( bval ) => {
//         val z = a.value + bval
//         cn( z * z )
//      }
//      case _ => super.sqrsum( b )
//   }
//   override def sqrdif( b: GE ) : GE   = b match {
//      case C( bval ) => {
//         val z = a.value - bval
//         cn( z * z )
//      }
//      case _ => super.sqrdif( b )
//   }
//   override def absdif( b: GE ) : GE   = b match {
//      case C( bval ) => cn( math.abs( a.value - bval ))
//      case _ => super.absdif( b )
//   }
//   override def thresh( b: GE ) : GE   = b match {
//      case C( bval ) => cn( if( a.value < bval ) 0 else a.value )
//      case _ => super.thresh( b )
//   }
//   override def amclip( b: GE ) : GE   = b match {
//      case C( bval ) => cn( a.value * 0.5f * (bval + math.abs( a.value )))
//      case _ => super.amclip( b )
//   }
//   override def scaleneg( b: GE ) : GE = b match {
//      case C( bval ) => cn( (math.abs( a.value ) - a.value) * (0.5f * bval + 0.5f) + a.value )
//      case _ => super.scaleneg( b )
//   }
//   override def clip2( b: GE ) : GE    = b match {
//      case C( bval ) => cn( math.max( math.min( a.value, bval ), -bval ))
//      case _ => super.clip2( b )
//   }
//   override def excess( b: GE ) : GE   = b match {
//      case C( bval ) => cn( a.value - math.max( math.min( a.value, bval ), -bval ))
//      case _ => super.excess( b )
//   }
//   override def fold2( b: GE ) : GE    = b match {
//      case C( bval ) => cn( fold( a.value, -bval, bval ))
//      case _ => super.fold2( b )
//   }
//   override def wrap2( b: GE ) : GE    = b match {
//      case C( bval ) => cn( wrap( a.value, -bval, bval ))
//      case _ => super.wrap2( b )
//   }
//   def firstarg( b: GE ) : GE = a
}