/*
 *  EnvGen.scala
 *  Tintantmare
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
package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._
import SC._
import GraphBuilder._

/**
 *  @version  0.10, 01-Jan-10
 */
object Done extends UGen1RArgs {
  def kr( src: GE ) : GE = make( src )
}
case class Done( src: UGenIn ) extends SingleOutUGen( src ) with ControlRated

object FreeSelf extends UGen1RArgs {
  def kr( in: GE ) : GE = make( in ) // we do not return in like sclang does
}
case class FreeSelf( in: UGenIn ) extends ZeroOutUGen( in ) with ControlRated

object PauseSelf extends UGen1RArgs {
  def kr( in: GE ) : GE = make( in ) // we do not return in like sclang does
}
case class PauseSelf( in: UGenIn ) extends ZeroOutUGen( in ) with ControlRated

// its output is its input (source)
object FreeSelfWhenDone extends UGen1RArgs {
  def kr( src: GE ) : GE = make( src )
}
case class FreeSelfWhenDone( src: UGenIn ) extends SingleOutUGen( src )
with ControlRated

// its output is its input (source)
object PauseSelfWhenDone extends UGen1RArgs {
  def kr( src: GE ) : GE = make( src )
}
case class PauseSelfWhenDone( src: UGenIn ) extends SingleOutUGen( src )
with ControlRated

// its output is its input (gate)
object Pause extends UGen2RArgs {
  def kr( gate: GE, nodeID: GE ) : GE = make( gate, nodeID )
}
case class Pause( gate: UGenIn, nodeID: UGenIn )
extends SingleOutUGen( gate, nodeID ) with ControlRated

// its output is its input (trig)
object Free extends UGen2RArgs {
  def kr( trig: GE, nodeID: GE ) : GE = make( trig, nodeID )
}
case class Free( trig: UGenIn, nodeID: UGenIn )
extends SingleOutUGen( trig, nodeID ) with ControlRated

object EnvGen {
  def ar( envelope: Env, gate: GE = 1, levelScale: GE = 1, levelBias: GE = 0,
          timeScale: GE = 1, doneAction: GE = doNothing ) : GE = {
//    val exp = expand( gate, levelScale, levelBias, timeScale, doneAction, envelope.toArray: _* )
    val exp = expand( (List( gate, levelScale, levelBias, timeScale, doneAction ) ++ envelope.toArray): _* )
    simplify( for( List( g, ls, lb, t, d, e @ _* ) <- exp) yield this( audio, g, ls, lb, t, d, e ))
//  ar( envelope.toArray, gate, levelScale, levelBias, timeScale, doneAction )
  }
  
//  def ar( envArray: Array[ GE ], gate: GE, levelScale: GE, levelBias: GE, timeScale: GE, doneAction: GE ) : GE = {
//  	UGen.multiNew( "EnvGen", audio, List( audio ), List( gate, levelScale, levelBias, timeScale, doneAction ) ++ envArray )
//  }
  
  def kr( envelope: Env, gate: GE = 1, levelScale: GE = 1, levelBias: GE = 0,
          timeScale: GE = 1, doneAction: GE = doNothing ) : GE = {
    val exp = expand( (List( gate, levelScale, levelBias, timeScale, doneAction ) ++ envelope.toArray): _* )
    simplify( for( List( g, ls, lb, t, d, e @ _* ) <- exp) yield this( control, g, ls, lb, t, d, e ))
  }
  
//  def kr( envArray: Array[ GE ], gate: GE, levelScale: GE, levelBias: GE, timeScale: GE, doneAction: GE ) : GE = {
//  	UGen.multiNew( "EnvGen", control, List( control ), List( gate, levelScale, levelBias, timeScale, doneAction ) ++ envArray )
//  }
}

case class EnvGen( rate: Rate, gate: UGenIn, levelScale: UGenIn, levelBias: UGenIn,
                   timeScale: UGenIn, doneAction: UGenIn, envSeq: Seq[ UGenIn ])
extends SingleOutUGen( (List( gate, levelScale, levelBias, timeScale, doneAction ) ++ envSeq): _* )

