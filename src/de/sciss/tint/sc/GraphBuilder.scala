/*
 *  Constant.scala
 *  GraphBuilder
 *
 *  Copyright (c) 2008-2009 Hanns Holger Rutz. All rights reserved.
 *
 *	This software is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU General Public License
 *	as published by the Free Software Foundation; either
 *	version 2, june 1991 of the License, or (at your option) any later version.
 *
 *	This software is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *	General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public
 *	License (gpl.txt) along with this software; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	For further information, please contact Hanns Holger Rutz at
 *	contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.tint.sc

import Predef._

/**
 * 	@version	0.12, 09-Dec-09
 */
trait GE {
  val numOutputs : Int
  def toUGenInputs : Seq[ UGenInput ]

  // binary ops
  def +( b: GE ) : GE     = BinaryOpUGen( Symbol( "+" ), this, b )
  def -( b: GE ) : GE     = BinaryOpUGen( Symbol( "-" ), this, b )
  def *( b: GE ) : GE     = BinaryOpUGen( Symbol( "*" ), this, b )
//  def div( b: GE ) : GE   = BinaryOpUGen( Symbol( 'div, this, b )
  def /( b: GE ) : GE     = BinaryOpUGen( Symbol( "/" ), this, b )
  def mod( b: GE ) : GE   = BinaryOpUGen( 'mod, this, b )
  def <( b: GE ) : GE	  = BinaryOpUGen( Symbol( "<" ), this, b )
  def >( b: GE ) : GE	  = BinaryOpUGen( Symbol( ">" ), this, b )
  def <=( b: GE ) : GE	  = BinaryOpUGen( Symbol( "<=" ), this, b )
  def >=( b: GE ) : GE	  = BinaryOpUGen( Symbol( ">=" ), this, b )
  def min( b: GE ) : GE   = BinaryOpUGen( 'min, this, b )
  def max( b: GE ) : GE   = BinaryOpUGen( 'max, this, b )
//  def &( b: GE ) : GE	  = BinaryOpUGen( Symbol( "&" ), this, b )
//  def |( b: GE ) : GE	  = BinaryOpUGen( Symbol( "|" ), this, b )
  def round( b: GE ) : GE = BinaryOpUGen( 'round, this, b )
  def clip2( b: GE ) : GE = BinaryOpUGen( 'clip2, this, b )

  // unary ops
  def neg : GE        = UnaryOpUGen( 'neg, this )
  def reciprocal : GE = UnaryOpUGen( 'reciprocal, this )
  def bitNot : GE	  = UnaryOpUGen( 'bitNot, this )
  def abs : GE	      = UnaryOpUGen( 'abs, this )
// def asFloat : GE	  = UnaryOpUGen( 'asFloat, this )
// def asInteger : GE	  = UnaryOpUGen( 'asInteger, this )
  def ceil : GE	      = UnaryOpUGen( 'ceil, this )
  def floor : GE	  = UnaryOpUGen( 'floor, this )
  def frac : GE	      = UnaryOpUGen( 'frac, this )
  def sign : GE	      = UnaryOpUGen( 'sign, this )
  def squared : GE    = UnaryOpUGen( 'squared, this )
  def cubed : GE      = UnaryOpUGen( 'cubed, this )
  def sqrt : GE       = UnaryOpUGen( 'sqrt, this )
  def exp : GE        = UnaryOpUGen( 'exp, this )
  def midicps : GE    = UnaryOpUGen( 'midicps, this )
  def cpsmidi : GE    = UnaryOpUGen( 'cpsmidi, this )
  def midiratio : GE  = UnaryOpUGen( 'midiratio, this )
  def ratiomidi : GE  = UnaryOpUGen( 'ratiomidi, this )
  def ampdb : GE      = UnaryOpUGen( 'ampdb, this )
  def dbamp : GE      = UnaryOpUGen( 'dbamp, this )
  def octcps : GE     = UnaryOpUGen( 'octcps, this )
  def cpsoct : GE     = UnaryOpUGen( 'cpsoct, this )
  def log : GE        = UnaryOpUGen( 'log, this )
  def log2 : GE       = UnaryOpUGen( 'log2, this )
  def log10 : GE      = UnaryOpUGen( 'log10, this )
  def sin : GE        = UnaryOpUGen( 'sin, this )
  def cos : GE        = UnaryOpUGen( 'cos, this )
  def tan : GE        = UnaryOpUGen( 'tan, this )
  def asin : GE       = UnaryOpUGen( 'asin, this )
  def acos : GE       = UnaryOpUGen( 'acos, this )
  def atan : GE       = UnaryOpUGen( 'atan, this )
  def sinh : GE       = UnaryOpUGen( 'sinh, this )
  def cosh : GE       = UnaryOpUGen( 'cosh, this )
  def tanh : GE       = UnaryOpUGen( 'tanh, this )
  def rand : GE       = UnaryOpUGen( 'rand, this )
  def rand2 : GE      = UnaryOpUGen( 'rand2, this )
  def linrand : GE    = UnaryOpUGen( 'linrand, this )
  def bilinrand : GE  = UnaryOpUGen( 'bilinrand, this )
  def sum3rand : GE   = UnaryOpUGen( 'sum3rand, this )
  def distort : GE    = UnaryOpUGen( 'distort, this )
  def softclip : GE   = UnaryOpUGen( 'softclip, this )
  def coin : GE       = UnaryOpUGen( 'coin, this )
  def even : GE       = UnaryOpUGen( 'even, this )
  def odd : GE        = UnaryOpUGen( 'odd, this )
  def rectWindow : GE = UnaryOpUGen( 'rectWindow, this )
  def hanWindow : GE  = UnaryOpUGen( 'hanWindow, this )
  def welWindow : GE  = UnaryOpUGen( 'sum3rand, this )
  def triWindow : GE  = UnaryOpUGen( 'triWindow, this )
  def scurve : GE     = UnaryOpUGen( 'scurve, this )
  def ramp : GE       = UnaryOpUGen( 'ramp, this )
  def isPositive : GE = UnaryOpUGen( 'isPositive, this )
  def isNegative : GE = UnaryOpUGen( 'isNegative, this )
  def isStrictlyPositive : GE = UnaryOpUGen( 'isStrictlyPositive, this )
  def rho : GE        = UnaryOpUGen( 'rho, this )
  def theta : GE      = UnaryOpUGen( 'theta, this )

  def madd( mul: GE ) : GE = { 		
    MulAdd( this, mul, Constants.zero )
  }
  
  def madd( mul: GE, add: GE ) : GE = { 		
    MulAdd( this, mul, add )
  }
}

case class GESeq( elements: Seq[ UGenInput ]) extends GE
{
  val numOutputs = elements.size
  def getOutputAt( idx: Int ) = elements( idx )
  def toUGenInputs : Seq[ UGenInput ] = elements
}

object GraphBuilder {
	//used to create an out ugen automatically and a fade envelope

    def seq( elements: Seq[ UGenInput ]) : GE = {
      if( elements.size == 1 ) elements.head else new GESeq( elements )
    }

 	def replaceZeroesWithSilence( input: GE ) : GE = {
		input match {
//			case x: OutputProxy => input
			case Constants.zero => Silent.ar( 1 )
//			case x: Constant => input
			case x: GESeq => {
				// this replaces zeroes with audio rate silence.
				// sub collections are deep replaced

				val ugenInputs = input.toUGenInputs
				val numZeroes = ugenInputs.filter( _ == Constants.zero ).size
				if( numZeroes == 0 ) {
					input
				} else {
					val silent = Silent.ar( numZeroes ).toUGenInputs
//		    val iter = ugenInputs.elements.counted
					var pos = 0
					// XXX recursion missing
					val result = ugenInputs map (ugenInput => {
						if( input == Constants.zero ) {
							pos = pos + 1
							silent( pos - 1 )
						} else ugenInput
					})
					new GESeq( result )
				}
			}
			case _ => input
		}
 	}
  
/*
    def rateForElem( elem: GE ) : Symbol = {
      if( elem.isInstanceOf[ UGenInput ])
        elem.asInstanceOf[ UGenInput ].rate
      else
        rateForElem( elem.getOutputAt( 0 ))
    }
*/
  
    def wrapOut( name: String, func: () => GE, rates: Seq[Any], prependArgs: Seq[Any], outClass: String = "Out", fadeTime: Option[Float] ) : SynthDef = {
		def fullFunc() : GE = {
			var result = func.apply() // .toUGenInputs
			val rate = Symbol( result.toUGenInputs.map(_.rate.name).max )
//			( new Ordering[ Symbol ] {
//				def compare( x: Symbol, y: Symbol ) : Int = {
//					x.name.compare( y.name )
//				}
//			})
			if( rate == 'scalar ) {
				result
			} else {
				fadeTime.foreach( fdt => {
					result = makeFadeEnv( fdt ) * result
				})
//				val i_out : UGenInput = Constant( 0 )
				val i_out = "i_out".ir
//				result = replaceZeroesWithSilence( result )
				UGen.multiNew( outClass, rate, Nil /* (1 to result.size) map (x => rate) */,
							   List( i_out ) ++ result.toUGenInputs )
			}
//			val rate='scalar // XXX
//			val rate = result.first.rate
//			val resultSeq: GESeq = result match {
//				case seq: GESeq => seq
//				case ugen: GE => GESeq( ugen )
//			}
		}
//		val res =
			new SynthDef( name, fullFunc )
//		res.writeDefFile
//		res
	}

	def makeFadeEnv( fadeTime: Float ) : GE = {
//		val dt			= ControlName( "fadeTime" ).kr( fadeTime )
		val dt			= "fadeTime".kr( fadeTime )
//		val gate		= ControlName( "gate" ).kr( 1 )
		val gate		= "gate".kr( 1 )
		val startVal	= (dt <= 0)
		EnvGen.kr( new Env( List( startVal, 1, 0 ), List( 1, 1 ), List( 1, 1 ), 1 ), gate, 1, 0, dt, 2 )
	}
}