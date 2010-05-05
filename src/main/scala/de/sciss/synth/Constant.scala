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
 *	@version	   0.12, 27-Apr-10
 */
case class Constant( value: Float ) extends UGenIn with ScalarRated {
   override def toString = value.toString

   @inline private def cn( f: Float )  = Constant( f )
   @inline private def cn( d: Double ) = Constant( d.toFloat )

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
}