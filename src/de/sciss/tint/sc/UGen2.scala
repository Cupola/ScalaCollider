/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.sciss.tint.sc

import Predef._
import Rates._

class UGen2 {

}

trait UGenFactory {
//  val name: String
//  def numOutputs = 1

  protected def expand( args: GE* ): List[ List[ UGenInput ]] = {
    val expanded = args.toList.map( (x) => List( x.toUGenInputs.head ))
    expanded
  }

  protected def simplify( res: List[ UGen ]) : GE = res match {
    case List( e ) => e
    case more => seqOfGE2GESeq( more )
  }
}

object Osc2 extends UGenFactory {
//  val name = "Osc"

//  def ar( bufNum: GE, freq: GE = 440, phase: GE = 0 ) : GE = {
//    expand( bufNum, freq, phase )( (each) => { val List( bufNum, freq, phase ) = each; ar1( bufNum, freq, phase )})
//  }

  def ar( bufNum: GE, freq: GE = 440, phase: GE = 0 ) : GE = {
    simplify( for( List( b, f, p ) <- expand( bufNum, freq, phase )) yield this( audio, b, f, p ))
  }

  def kr( bufNum: GE, freq: GE = 440, phase: GE = 0 ) : GE = {
    simplify( for( List( b, f, p ) <- expand( bufNum, freq, phase )) yield this( control, b, f, p ))
  }

  def apply( rate: Rate, bufNum: UGenInput, freq: UGenInput, phase: UGenInput ) = new Osc2( rate, bufNum, freq, phase )

//  def ar1( bufNum: UGenInput, freq: UGenInput, phase: UGenInput ) : UGen =

//  def kr( bufNum: GE, freq: GE = 440, phase: GE = 0 ) : GE = {
//    multiNew( control, List( control ), List( bufNum, freq, phase ))
//  }
}

class Osc2 private ( r: Rate, val bufNum: UGenInput, val freq: UGenInput, val phase: UGenInput )
extends SingleOutUGen( "Osc", r, r, List( bufNum, freq, phase ))

object SinOsc2 extends UGenFactory {
  def ar( freq: GE = 440, phase: GE = 0 ) : GE = {
    simplify( for( List( f, p ) <- expand( freq, phase )) yield this( audio, f, p ))
  }

  def kr( freq: GE = 440, phase: GE = 0 ) : GE = {
    simplify( for( List( f, p ) <- expand( freq, phase )) yield this( control, f, p ))
  }

  def apply( rate: Rate, freq: UGenInput, phase: UGenInput ) = new SinOsc2( rate, freq, phase )

//  def ar1( bufNum: UGenInput, freq: UGenInput, phase: UGenInput ) : UGen =

//  def kr( bufNum: GE, freq: GE = 440, phase: GE = 0 ) : GE = {
//    multiNew( control, List( control ), List( bufNum, freq, phase ))
//  }
}

class SinOsc2 private ( r: Rate, val freq: UGenInput, val phase: UGenInput )
extends SingleOutUGen( "SinOsc", r, r, List( freq, phase ))