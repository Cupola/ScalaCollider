/*
 *  BasicOpsUGen.scala
 *  Tintantmare
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

import _root_.scala.math._

object MulAdd {
//  private def multiNew( name: String, rate: Symbol, outputRates: Seq[ Symbol ], inputs: Seq[ GE ]) : GE = {... }
  private def findHighestRate( inputs: Seq[ UGenInput ]) : Symbol = {
    var mx = 0
    inputs.foreach (input => { mx = max( mx, UGen.getRateID( input.rate ))})
    UGen.getRateSymbol( mx )
  }
  
  def apply( in: GE, mul: GE, add: GE ) : GE = {
    var chanExp = 0;
    var allOne = true
    var hasZero = false
    val inputs = List( in, mul, add )
    
    for( input <- inputs ) {
      chanExp = max( chanExp, input.numOutputs )
      allOne  = allOne && (input.numOutputs == 1)
      hasZero = hasZero || (input.numOutputs == 0)
    }
    if( hasZero ) return new GESeq( Nil )	// cannot wrap zero size seq
    if( allOne ) {
 //	  val ugenInputs = inputs.flatMap (_.toUGenInputs )
      val ini  = in.toUGenInputs.head
      val muli = mul.toUGenInputs.head
      val addi = add.toUGenInputs.head
      return optimizedNew( ini, muli, addi )
     }

    val results = new Array[ GE ]( chanExp )
//  val ugenInputs = inputs map (_.toUGenInputs)
    val inis  = in.toUGenInputs
    val mulis = mul.toUGenInputs
    val addis = add.toUGenInputs
        
    for( chan <- (0 until chanExp)) {
//    val newArgs = ugenInputs map (multiInput => multiInput( chan % ugenInputs.size ))
//    results.update( chan, new UGen( name, rate, outputRates, newArgs ));
//	  results.update( chan, optimizedNew( newArgs ))
      val ini  = inis( chan % inis.size )
      val muli = mulis( chan % mulis.size )
      val addi = addis( chan % addis.size )
      results.update( chan, optimizedNew( ini, muli, addi ))
    }
    val res2 = results flatMap (_.toUGenInputs)
    GraphBuilder.seq( res2 )
  }
  
  private def optimizedNew( in: UGenInput, mul: UGenInput, add: UGenInput ) : GE = {
	// eliminate degenerate cases
    if( mul == Constants.zero ) return add
    val minus = mul == Constants.minusOne
    val nomul = mul == Constants.one
    val noadd = add == Constants.zero

    if( nomul && noadd ) return in
    if( minus && noadd ) return in.neg
    if( noadd ) return in * mul
    if( minus ) return add - in
    if( nomul ) return in + add
    
    // do the full ugen
    val inputs = List( in, mul, add )
    val rate = findHighestRate( inputs )
    return new SingleOutUGen( "MulAdd", rate, rate, inputs )
  }

  def canBeMulAdd( in: RatedGE, mul: RatedGE, add: RatedGE ) : Boolean = {
    // see if these inputs satisfy the constraints of a MulAdd ugen.
    if( in.rate == 'audio ) return true
    if( (in.rate == 'control) && ((mul.rate == 'control) || (mul.rate == 'scalar)) && 
          ((add.rate == 'control) || (add.rate == 'scalar)) ) return true 
    false
  }
}

class BasicOpUGen( override val name: String, override val specialIndex: Int, override val rate: Symbol,
                       override val inputs: Seq[ UGenInput ] )
extends SingleOutUGen( name, rate, rate, inputs );

object UnaryOpUGen {
  private val selectors = List(
    'neg, 'not, 'isNil, 'notNil, 'bitNot, 'abs, 'asFloat, 'asInteger, 'ceil, 'floor, 'frac, 'sign,
    'squared, 'cubed, 'sqrt, 'exp, 'reciprocal, 'midicps, 'cpsmidi, 'midiratio, 'ratiomidi,
    'dbamp, 'ampdb, 'octcps, 'cpsoct, 'log, 'log2, 'log10, 'sin, 'cos, 'tan, 'asin, 'acos, 'atan,
    'sinh, 'cosh, 'tanh, 'rand, 'rand2, 'linrand, 'bilinrand, 'sum3rand, 'distort, 'softclip,
    'coin, 'digitValue, 'silence, 'GAGA, 'rectWindow, 'hanWindow, 'welWindow, 'triWindow, 'ramp, 'scurve
  )

  def apply( selector: Symbol, a: GE ) : GE = {
    val chanExp = a.numOutputs
    val allOne	= a.numOutputs == 1
    val hasZero = a.numOutputs == 0
    
    if( hasZero ) return new GESeq( Nil )	// cannot wrap zero size seq
    if( allOne ) {
 //	  val ugenInputs = inputs.flatMap (_.toUGenInputs )
      val ai = a.toUGenInputs.head
      return optimizedNew( selector, ai )
     }

    val results = new Array[ GE ]( chanExp )
    val ais  = a.toUGenInputs
        
    for( chan <- (0 until chanExp)) {
      val ai = ais( chan % ais.size )
      results.update( chan, optimizedNew( selector, ai ))
    }
    val res2 = results flatMap (_.toUGenInputs)
    GraphBuilder.seq( res2 )
  }

  private def optimizedNew( selector: Symbol, a: UGenInput ) : GE = {
    // replace constants immediately
    if( a.isInstanceOf[ Constant ]) {
      val aval = a.asInstanceOf[ Constant ].value
      val bval : Double = selector match {
	      case 'neg => -aval
	//    case 'not =>
	      case 'isNil => 0f
	      case 'notNil => 1f
	      case 'bitNot => aval.toInt ^ -1
	      case 'abs => abs( aval )
	      case 'asFloat => aval
	      case 'asInteger => aval.toInt
	      case 'ceil => ceil( aval )
	      case 'floor => floor( aval )
	      case 'frac => aval % 1.0	// XXX OK for negative values?
	      case 'sign => if( aval == 0 ) 0 else if( aval < 0 ) -1 else 1
	      case 'squared => aval * aval
	      case 'cubed => aval * aval * aval
	      case 'sqrt => sqrt( aval )
	      case 'exp => exp( aval )
	      case 'reciprocal => 1.0f / aval
	      case 'midicps => 440 * pow( 2, (aval - 69) * 0.083333333333 )
	      case 'cpsmidi => log( aval * 0.0022727272727 ) / log( 2 ) * 12 + 69
	      case 'midiratio => pow( 2, aval * 0.083333333333 )
	      case 'ratiomidi => 12 * log( aval ) / log( 2 )
	      case 'dbamp => pow( 10, aval * 0.05 )
	      case 'ampdb => log( aval ) / log( 10 )* 20
	      case 'octcps => 440 * pow( 2, aval - 4.75 )
	      case 'cpsoct => log( aval * 0.0022727272727 ) / log( 2 ) + 4.75
	      case 'log => log( aval )
	      case 'log2 => log( aval ) / log( 2 )
	      case 'log10 => log( aval ) / log( 10 )
	      case 'sin => sin( aval )
	      case 'cos => cos( aval )
	      case 'tan => tan( aval )
	      case 'asin => asin( aval )
	      case 'acos => acos( aval )
	      case 'atan => atan( aval )
//	      case 'sinh => Math.sinh( aval )	// XXX
//	      case 'cosh => Math.cosh( aval )	// XXX
//	      case 'tanh => Math.tanh( aval )	// XXX
//	      case 'rand => Math.rand( aval ) // XXX seed / Routine
//	      case 'rand2 => Math.rand( -aval, aval ) // XXX seed / Routine
	//      case 'linrand =>	// XXX
	//      case 'bilinrand =>	// XXX
	//      case 'sum3rand =>	// XXX
	      case 'distort => aval / (1 + abs( aval ))
	      case 'softclip =>	{ val absx = abs( aval ); if( absx <= 0.5 ) aval; else (absx - 0.25) / aval }
//	      case 'coin => if( Math.rand() < aval ) 1 else 0	// XXXX
	//    case 'digitValue =>	// XXX
	      case 'silence => 0f
	      case 'thru => aval
	      case 'rectWindow => if( (aval < 0) || (aval > 1) ) 0 else 1
	      case 'hanWindow => if( (aval < 0) || (aval > 1) ) 0 else 0.5 - 0.5 * cos( aval * Pi * 2 )
	      case 'welWindow => if( (aval < 0) || (aval > 1) ) 0 else sin( aval * Pi )
	      case 'triWindow => if( (aval < 0) || (aval > 1) ) 0 else if( aval < 0.5 ) 2 * aval else -2 * aval + 2
	      case 'ramp =>	if( aval <= 0 ) 0 else if( aval >= 1 ) 1 else aval
	      case 'scurve => if( aval <= 0 ) 0 else if( aval > 1 ) 1 else aval * aval * (3 - 2 * aval)
	      case _ => {
	        // do the full ugen
	        val result = new SingleOutUGen( "UnaryOpUGen", a.rate, a.rate, List( a ))
	        result.synthIndex = selectors.indexOf( selector )
	        return result
	      }
      }
      return Constant( bval.toFloat )
    }
    // do the full ugen
    val result = new BasicOpUGen( "UnaryOpUGen", selectors.indexOf( selector ), a.rate, List( a ));
    result
  }

  def determineRate( a: UGenInput, b: UGenInput ) : Symbol = {
    if( a.rate == 'demand ) return 'demand
    if( b.rate == 'demand ) return 'demand
    if( a.rate == 'audio ) return 'audio
    if( b.rate == 'audio ) return 'audio
    if( a.rate == 'control ) return 'control
    if( b.rate == 'control ) return 'control
    'scalar
  }
}

object BinaryOpUGen {
  private val selectors = List(
	Symbol( "+" ), Symbol( "-" ), Symbol( "*" ), 'div, Symbol( "/" ),
	'mod, '== , Symbol( "!=" ), Symbol( "<" ) , Symbol( ">" ),
    Symbol( "<=" ), Symbol( ">=" ), 'min, 'max, Symbol( "&" ), Symbol( "|" ), Symbol( "^" ),
    'lcm, 'gcd, 'round, 'roundUp, 'trunc, 'atan2, 'hypot, 'hypotApx, 'pow, 'leftShift, 'rightShift, 
    'unsignedRightShift, 'fill, 'ring1, 'ring2, 'ring3, 'ring4, 'difsqr, 'sumsqr, 'sqrsum, 'sqrdif,
    'absdif, 'thresh, 'amclip, 'scaleneg, 'clip2, 'excess, 'fold2, 'wrap2, 'firstArg, 'rrand, 'exprand
  )
  
  def apply( selector: Symbol, a: GE, b: GE ) : GE = {
    var chanExp = 0;
    var allOne = true
    var hasZero = false
    val inputs = List( a, b )
    
    for( input <- inputs ) {
      chanExp = max( chanExp, input.numOutputs )
      allOne  = allOne && (input.numOutputs == 1)
      hasZero = hasZero || (input.numOutputs == 0)
    }
    if( hasZero ) return new GESeq( Nil )	// cannot wrap zero size seq
    if( allOne ) {
 //	  val ugenInputs = inputs.flatMap (_.toUGenInputs )
      val ai  = a.toUGenInputs.head
      val bi  = b.toUGenInputs.head
      return optimizedNew( selector, ai, bi )
     }

    val results = new Array[ GE ]( chanExp )
//  val ugenInputs = inputs map (_.toUGenInputs)
    val ais  = a.toUGenInputs
    val bis	 = b.toUGenInputs
        
    for( chan <- (0 until chanExp)) {
//    val newArgs = ugenInputs map (multiInput => multiInput( chan % ugenInputs.size ))
//    results.update( chan, new UGen( name, rate, outputRates, newArgs ));
//	  results.update( chan, optimizedNew( newArgs ))
      val ai  = ais( chan % ais.size )
      val bi  = bis( chan % bis.size )
      results.update( chan, optimizedNew( selector, ai, bi ))
    }
    val res2 = results flatMap (_.toUGenInputs)
    GraphBuilder.seq( res2 )
  }

  private def optimizedNew( selector: Symbol, a: UGenInput, b: UGenInput ) : GE = {
//    if( a.isInstanceOf[ Constant ] && b.isInstanceOf[ Constant ]) {
      // XXX
//    }
    
    // eliminate degenerate cases
    if( selector == Symbol( "*" )) {
      if( a == Constants.zero ) return Constants.zero
      if( b == Constants.zero ) return Constants.zero
      if( a == Constants.one ) return b
      if( a == Constants.minusOne ) return b.neg
      if( b == Constants.one ) return a
      if( b == Constants.minusOne ) return a.neg
    } else if( selector == Symbol( "+" )) {
      if( a == Constants.zero ) return b
      if( b == Constants.zero ) return a
    } else if( selector == Symbol( "-" )) {
      if( a == Constants.zero ) return b.neg
      if( b == Constants.zero ) return a
    } else if( selector == Symbol( "/" )) {
      if( b == Constants.one ) return a
      if( b == Constants.minusOne ) return a.neg
      if( b.rate == 'scalar ) return( a * b.reciprocal )
    }

    // do the full ugen
    val rate = determineRate( a, b )
    val result = new BasicOpUGen( "BinaryOpUGen", selectors.indexOf( selector ), rate, List( a, b ));
//    println( "index of " + selector + " is " + result.synthIndex )
    result
  }

  private def determineRate( a: UGenInput, b: UGenInput ) : Symbol = {
    if( a.rate == 'demand ) return 'demand
    if( b.rate == 'demand ) return 'demand
    if( a.rate == 'audio ) return 'audio
    if( b.rate == 'audio ) return 'audio
    if( a.rate == 'control ) return 'control
    if( b.rate == 'control ) return 'control
    'scalar
  }
}
