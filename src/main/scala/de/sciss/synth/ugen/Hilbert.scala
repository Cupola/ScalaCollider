/*
 *  Hilbert.scala
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

import de.sciss.synth.{ audio, GE, MultiOutUGen, Rate, SingleOutUGen, UGenIn }

/**
 *	@version	0.10, 22-Apr-10
 */
object Hilbert extends UGen1Args { // do not use UGen1RArgs as there might be a kr version in the future
  def ar( in: GE ) : GE = arExp( in )
}
case class Hilbert( rate: Rate, in: UGenIn )
extends MultiOutUGen( audio, 2, List( in ))

object FreqShift extends UGen3Args {
  def ar( in: GE, freq: GE = 0, phase: GE = 0 ) : GE = arExp( in, freq, phase )
  def kr( in: GE, freq: GE = 0, phase: GE = 0 ) : GE = krExp( in, freq, phase )
}
case class FreqShift( rate: Rate, in: UGenIn, freq: UGenIn, phase: UGenIn )
extends SingleOutUGen( in, freq, phase )
