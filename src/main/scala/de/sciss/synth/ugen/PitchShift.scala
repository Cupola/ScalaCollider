/*
 *  PitchShift.scala
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

import de.sciss.synth.{ AudioRated, GE, SingleOutUGen, UGenIn }

/**
 *  @version  0.10, 01-Jan-10
 */
object PitchShift extends UGen5RArgs {
  def ar( in: GE, winSize: GE = 0.2f, pitchRatio: GE = 1,
          pitchDispersion: GE = 0, timeDispersion: GE = 0 ) : GE =
    make( in, winSize, pitchRatio, pitchDispersion, timeDispersion )
}
case class PitchShift( in: UGenIn, winSize: UGenIn, pitchRatio: UGenIn,
                       pitchDispersion: UGenIn, timeDispersion: UGenIn )
extends SingleOutUGen( in, winSize, pitchRatio, pitchDispersion, timeDispersion )
with AudioRated