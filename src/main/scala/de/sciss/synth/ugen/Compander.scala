/*
 *  Compander.scala
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
import GraphBuilder._

/**
 *	@version	0.10, 01-Jan-10
 */
object Amplitude extends UGen3Args {
  def ar( in: GE, attack: GE = 0.01f, release: GE = 0.01f ) =
    arExp( in, attack, release )

  def kr( in: GE, attack: GE = 0.01f, release: GE = 0.01f ) =
    krExp( in, attack, release )
}
case class Amplitude( rate: Rate, in: UGenIn, attack: UGenIn, release: UGenIn )
extends SingleOutUGen( in, attack, release )

object Compander extends UGen7Args {
  def ar( in: GE, control: GE = 0, thresh: GE = 0.5f, ratioBelow: GE = 1,
          ratioAbove: GE = 1, attack: GE = 0.01f, release: GE = 0.01f ) =
    arExp( in, control, thresh, ratioBelow, ratioAbove, attack, release )

  def kr( in: GE, control: GE = 0, thresh: GE = 0.5f, ratioBelow: GE = 1,
          ratioAbove: GE = 1, attack: GE = 0.01f, release: GE = 0.01f ) =
    krExp( in, control, thresh, ratioBelow, ratioAbove, attack, release )
}
case class Compander( rate: Rate, in: UGenIn, control: UGenIn, thresh: UGenIn,
                      ratioBelow: UGenIn, ratioAbove: UGenIn, attack: UGenIn,
                      release: UGenIn )
extends SingleOutUGen( in, control, thresh, ratioBelow, ratioAbove, attack, release )

// _not_ a ugen
object CompanderD {
  def ar( in: GE, thresh: GE = 0.5f, ratioBelow: GE = 1,
          ratioAbove: GE = 1, attack: GE = 0.01f, release: GE = 0.01f ) =
    Compander.ar( DelayN.ar( in, attack, attack ), in, thresh,
                  ratioBelow, ratioAbove, attack, release )
}

object Normalizer extends UGen3Args {
  def ar( in: GE, level: GE = 1, dur: GE = 0.01f ) = arExp( in, level, dur )
  def kr( in: GE, level: GE = 1, dur: GE = 0.01f ) = krExp( in, level, dur )
}
case class Normalizer( rate: Rate, in: UGenIn, level: UGenIn, dur: UGenIn )
extends SingleOutUGen( in, level, dur )

object Limiter extends UGen3Args {
  def ar( in: GE, level: GE = 1, dur: GE = 0.01f ) = arExp( in, level, dur )
  def kr( in: GE, level: GE = 1, dur: GE = 0.01f ) = krExp( in, level, dur )
}
case class Limiter( rate: Rate, in: UGenIn, level: UGenIn, dur: UGenIn )
extends SingleOutUGen( in, level, dur )
