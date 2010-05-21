/*
 *  EnvGen.scala
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

package de.sciss.synth.ugen

import de.sciss.synth._
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
case class FreeSelf( in: UGenIn ) extends SingleOutUGen( in ) with ControlRated with SideEffectUGen

object PauseSelf extends UGen1RArgs {
  def kr( in: GE ) : GE = make( in ) // we do not return in like sclang does
}
case class PauseSelf( in: UGenIn ) extends SingleOutUGen( in ) with ControlRated with SideEffectUGen

// its output is its input (source)
object FreeSelfWhenDone extends UGen1RArgs {
  def kr( src: GE ) : GE = make( src )
}
case class FreeSelfWhenDone( src: UGenIn ) extends SingleOutUGen( src )
with ControlRated with SideEffectUGen

// its output is its input (source)
object PauseSelfWhenDone extends UGen1RArgs {
  def kr( src: GE ) : GE = make( src )
}
case class PauseSelfWhenDone( src: UGenIn ) extends SingleOutUGen( src )
with ControlRated with SideEffectUGen

// its output is its input (gate)
object Pause extends UGen2RArgs {
  def kr( gate: GE, nodeId: GE ) : GE = make( gate, nodeId )
}
case class Pause( gate: UGenIn, nodeId: UGenIn )
extends SingleOutUGen( gate, nodeId ) with ControlRated with SideEffectUGen

// its output is its input (trig)
object Free extends UGen2RArgs {
  def kr( trig: GE, nodeId: GE ) : GE = make( trig, nodeId )
}
case class Free( trig: UGenIn, nodeId: UGenIn )
extends SingleOutUGen( trig, nodeId ) with ControlRated with SideEffectUGen

object EnvGen {
  def ar( envelope: Env, gate: GE = 1, levelScale: GE = 1, levelBias: GE = 0,
          timeScale: GE = 1, doneAction: GE = doNothing ) : GE = {
    val exp = expand( (List( gate, levelScale, levelBias, timeScale, doneAction ) ::: envelope.toList): _* )
    simplify( for( List( g, ls, lb, t, d, e @ _* ) <- exp) yield this( audio, g, ls, lb, t, d, e ))
  }
  
  def kr( envelope: Env, gate: GE = 1, levelScale: GE = 1, levelBias: GE = 0,
          timeScale: GE = 1, doneAction: GE = doNothing ) : GE = {
    val exp = expand( (List( gate, levelScale, levelBias, timeScale, doneAction ) ::: envelope.toList): _* )
    simplify( for( List( g, ls, lb, t, d, e @ _* ) <- exp) yield this( control, g, ls, lb, t, d, e ))
  }
}

case class EnvGen( rate: Rate, gate: UGenIn, levelScale: UGenIn, levelBias: UGenIn,
                   timeScale: UGenIn, doneAction: UGenIn, envSeq: Seq[ UGenIn ])
extends SingleOutUGen( (List( gate, levelScale, levelBias, timeScale, doneAction ) ++ envSeq): _* )
with SideEffectUGen  // side-effect: done action

object IEnvGen {
  def ar( envelope: IEnv, index: GE ) : GE = {
    val exp = expand( (List( index ) ::: envelope.toList): _* )
    simplify( for( List( i, e @ _* ) <- exp) yield this( audio, i, e ))
  }

  def kr( envelope: IEnv, index: GE ) : GE = {
    val exp = expand( (List( index ) ::: envelope.toList): _* )
    simplify( for( List( i, e @ _* ) <- exp) yield this( control, i, e ))
  }
}

case class IEnvGen( rate: Rate, index: UGenIn, ienvSeq: Seq[ UGenIn ])
extends SingleOutUGen( (List( index ) ++ ienvSeq): _* )

object Linen extends UGen5Args {
   def ar : GE = ar()
	def ar( gate: GE = 1, attack: GE = 0.01f, sustain: GE = 1, release: GE = 1,
            doneAction: GE = doNothing ) : GE =
      arExp( gate, attack, sustain, release, doneAction )

   def kr : GE = ar()
	def kr( gate: GE = 1, attack: GE = 0.01f, sustain: GE = 1, release: GE = 1,
            doneAction: GE = doNothing ) : GE =
      krExp( gate, attack, sustain, release, doneAction )
}
case class Linen( rate: Rate, gate: UGenIn, attack: UGenIn, sustain: UGenIn,
                  release: UGenIn, doneAction: UGenIn )
extends SingleOutUGen( gate, attack, sustain, release, doneAction )
with SideEffectUGen // side-effect: done-action