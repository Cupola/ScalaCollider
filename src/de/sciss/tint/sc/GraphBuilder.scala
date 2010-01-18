/*
 *  Constant.scala
 *  GraphBuilder
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
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

import SC._
import ugen.{ BinaryOpUGen => BinOp, EnvGen, MulAdd, Silent, Out, UnaryOpUGen => UnOp }
//import Rates._

/**
 * 	@version	0.13, 18-Jan-10
 */
trait GE {
  val numOutputs : Int
  def toUGenIns : Seq[ UGenIn ]

  def apply( idx: Int ) : UGenIn = toUGenIns( idx )

  // binary ops
  def +( b: GE ) : GE     = BinOp.make( Symbol( "+" ), this, b )
  def -( b: GE ) : GE     = BinOp.make( Symbol( "-" ), this, b )
  def *( b: GE ) : GE     = BinOp.make( Symbol( "*" ), this, b )
//  def div( b: GE ) : GE   = BinOp.make( Symbol( 'div, this, b )
  def /( b: GE ) : GE     = BinOp.make( Symbol( "/" ), this, b )
  def mod( b: GE ) : GE   = BinOp.make( 'mod, this, b )
  def <( b: GE ) : GE	  = BinOp.make( Symbol( "<" ), this, b )
  def >( b: GE ) : GE	  = BinOp.make( Symbol( ">" ), this, b )
  def <=( b: GE ) : GE	  = BinOp.make( Symbol( "<=" ), this, b )
  def >=( b: GE ) : GE	  = BinOp.make( Symbol( ">=" ), this, b )
  def min( b: GE ) : GE   = BinOp.make( 'min, this, b )
  def max( b: GE ) : GE   = BinOp.make( 'max, this, b )
//  def &( b: GE ) : GE	  = BinOp.make( Symbol( "&" ), this, b )
//  def |( b: GE ) : GE	  = BinOp.make( Symbol( "|" ), this, b )
  def round( b: GE ) : GE = BinOp.make( 'round, this, b )
  def clip2( b: GE ) : GE = BinOp.make( 'clip2, this, b )

  // unary ops
  def neg : GE        = UnOp.make( 'neg, this )
  def reciprocal : GE = UnOp.make( 'reciprocal, this )
  def bitNot : GE	  = UnOp.make( 'bitNot, this )
  def abs : GE	      = UnOp.make( 'abs, this )
// def asFloat : GE	  = UnOp.make( 'asFloat, this )
// def asInteger : GE	  = UnOp.make( 'asInteger, this )
  def ceil : GE	      = UnOp.make( 'ceil, this )
  def floor : GE	  = UnOp.make( 'floor, this )
  def frac : GE	      = UnOp.make( 'frac, this )
  def sign : GE	      = UnOp.make( 'sign, this )
  def squared : GE    = UnOp.make( 'squared, this )
  def cubed : GE      = UnOp.make( 'cubed, this )
  def sqrt : GE       = UnOp.make( 'sqrt, this )
  def exp : GE        = UnOp.make( 'exp, this )
  def midicps : GE    = UnOp.make( 'midicps, this )
  def cpsmidi : GE    = UnOp.make( 'cpsmidi, this )
  def midiratio : GE  = UnOp.make( 'midiratio, this )
  def ratiomidi : GE  = UnOp.make( 'ratiomidi, this )
  def ampdb : GE      = UnOp.make( 'ampdb, this )
  def dbamp : GE      = UnOp.make( 'dbamp, this )
  def octcps : GE     = UnOp.make( 'octcps, this )
  def cpsoct : GE     = UnOp.make( 'cpsoct, this )
  def log : GE        = UnOp.make( 'log, this )
  def log2 : GE       = UnOp.make( 'log2, this )
  def log10 : GE      = UnOp.make( 'log10, this )
  def sin : GE        = UnOp.make( 'sin, this )
  def cos : GE        = UnOp.make( 'cos, this )
  def tan : GE        = UnOp.make( 'tan, this )
  def asin : GE       = UnOp.make( 'asin, this )
  def acos : GE       = UnOp.make( 'acos, this )
  def atan : GE       = UnOp.make( 'atan, this )
  def sinh : GE       = UnOp.make( 'sinh, this )
  def cosh : GE       = UnOp.make( 'cosh, this )
  def tanh : GE       = UnOp.make( 'tanh, this )
  def rand : GE       = UnOp.make( 'rand, this )
  def rand2 : GE      = UnOp.make( 'rand2, this )
  def linrand : GE    = UnOp.make( 'linrand, this )
  def bilinrand : GE  = UnOp.make( 'bilinrand, this )
  def sum3rand : GE   = UnOp.make( 'sum3rand, this )
  def distort : GE    = UnOp.make( 'distort, this )
  def softclip : GE   = UnOp.make( 'softclip, this )
  def coin : GE       = UnOp.make( 'coin, this )
  def even : GE       = UnOp.make( 'even, this )
  def odd : GE        = UnOp.make( 'odd, this )
  def rectWindow : GE = UnOp.make( 'rectWindow, this )
  def hanWindow : GE  = UnOp.make( 'hanWindow, this )
  def welWindow : GE  = UnOp.make( 'sum3rand, this )
  def triWindow : GE  = UnOp.make( 'triWindow, this )
  def scurve : GE     = UnOp.make( 'scurve, this )
  def ramp : GE       = UnOp.make( 'ramp, this )
  def isPositive : GE = UnOp.make( 'isPositive, this )
  def isNegative : GE = UnOp.make( 'isNegative, this )
  def isStrictlyPositive : GE = UnOp.make( 'isStrictlyPositive, this )
  def rho : GE        = UnOp.make( 'rho, this )
  def theta : GE      = UnOp.make( 'theta, this )

//  def madd( mul: GE, add: GE ) : GE

  def madd( mul: GE, add: GE ) : GE = {
    Rates.highest( toUGenIns.map( _.rate ): _* ) match {
      case `audio`   => MulAdd.ar( this, mul, add )
      case `control` => MulAdd.kr( this, mul, add )
      case r => error( "Illegal rate " + r )
    }
  }
}

case class GESeq( elements: UGenIn* ) extends GE
{
  val numOutputs = elements.size
//  def getOutputAt( idx: Int ) = elements( idx )
  def toUGenIns : Seq[ UGenIn ] = elements

  override def toString = elements.mkString( "[ ", ", ", " ]" )
}

object GraphBuilder {
	//used to create an out ugen automatically and a fade envelope

    def seq( elements: UGenIn* ) : GE = {
      if( elements.size == 1 ) elements.head else new GESeq( elements: _* )
    }
/*
 	def replaceZeroesWithSilence( input: GE ) : GE = {
		input match {
//			case x: OutputProxy => input
			case Constants.zero => Silent.ar( 1 )
//			case x: Constant => input
			case x: GESeq => {
				// this replaces zeroes with audio rate silence.
				// sub collections are deep replaced

				val UGenIns = input.toUGenIns
				val numZeroes = UGenIns.filter( _ == Constants.zero ).size
				if( numZeroes == 0 ) {
					input
				} else {
					val silent = Silent.ar( numZeroes ).toUGenIns
//		    val iter = UGenIns.elements.counted
					var pos = 0
					// XXX recursion missing
					val result = UGenIns map (UGenIn => {
						if( input == Constants.zero ) {
							pos = pos + 1
							silent( pos - 1 )
						} else UGenIn
					})
					new GESeq( result: _* )
				}
			}
			case _ => input
		}
 	}
*/
/*
    def rateForElem( elem: GE ) : Symbol = {
      if( elem.isInstanceOf[ UGenIn ])
        elem.asInstanceOf[ UGenIn ].rate
      else
        rateForElem( elem.getOutputAt( 0 ))
    }
*/
  
    def wrapOut( name: String, func: () => GE, fadeTime: Option[Float] ) : SynthDef = {
		def fullFunc() : GE = {
			var result = func.apply() // .toUGenIns
			val rate = Rates.highest( result.toUGenIns.map( _.rate ): _* )
//			( new Ordering[ Symbol ] {
//				def compare( x: Symbol, y: Symbol ) : Int = {
//					x.name.compare( y.name )
//				}
//			})
			if( (rate == audio) || (rate == control) ) {
				fadeTime.foreach( fdt => {
					result = makeFadeEnv( fdt ) * result
				})
//				val i_out : UGenIn = Constant( 0 )
				val i_out = "i_out".ir
//				result = replaceZeroesWithSilence( result )

//                UGen.multiNew( outClass, rate, Nil /* (1 to result.size) map (x => rate) */,
//							   List( i_out ) ++ result.toUGenIns )

                if( rate == audio ) {
                  Out.ar( i_out, result )
                } else {
                  Out.kr( i_out, result )
                }
        
			} else {
            	result
            }
//			val rate='scalar // XXX
//			val rate = result.first.rate
//			val resultSeq: GESeq = result match {
//				case seq: GESeq => seq
//				case ugen: GE => GESeq( ugen )
//			}
		}
//		val res =
			new SynthDef( name )( fullFunc )
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

  def expand( args: GE* ): Seq[ List[ UGenIn ]] = {
    var chanExp = 0
    var allOne  = true
    var hasZero = false
    for( arg <- args ) {
      chanExp = scala.Math.max( chanExp, arg.numOutputs ) // shitty implicits don't work properly
      allOne  = allOne && (arg.numOutputs == 1)
      hasZero = hasZero || (arg.numOutputs == 0)
    }
//    println( "chanExp " + chanExp + "; allOne " + allOne + "; hasZero " + hasZero )
    if( allOne ) {
      List( args.toList.flatMap( _.toUGenIns.toList ))
    } else if( hasZero ) {
      Nil	// cannot wrap zero size seq
    } else {
      val exp  = args.toList.map( _.toUGenIns.toArray )
      for( ch <- 0 until chanExp ) yield exp.map( (arr) => arr.apply( ch % arr.size ))
    }
  }

  def simplify( res: Seq[ GE ]) : GE = { // UGenIn
//    println( "simplify : " + res )
    if( res.size == 1 ) {
      res.head
    } else {
      seqOfGE2GESeq( res )
    }
  }

  def replaceZeroesWithSilence( ge: GE ) : GE = {
    val ins = ge.toUGenIns
    val numZeroes = ins.foldLeft( 0 )( (sum, in) => in match {
        case Constant( 0 ) => sum + 1
        case _ => sum
    })
    if( numZeroes == 0 ) {
      ge
    } else {
      val silent = Silent.ar( numZeroes ).toUGenIns.iterator
      val res = ins map (in => in match {
          case Constant( 0 ) => silent.next
          case _ => in
      })
      simplify( res )
   }
  }
}