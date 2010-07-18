/*
 *  Gendyn.scala
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

import de.sciss.synth.{ Constant => c, _ }
import SynthGraph._

object Gendy1 extends UGen10ArgsIndiv {
  def ar( ampDist: GE = 1, durDist: GE = 1, adParam: GE = 1, ddParam: GE = 1,
          minFreq: GE = 440, maxFreq: GE = 660, ampScale: GE = 0.5f, durScale: GE = 0.5f,
          initCPs: GE = 12, kNum: GE = 12 ) : GE =
    arExp( ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum )

   def kr( ampDist: GE = 1, durDist: GE = 1, adParam: GE = 1, ddParam: GE = 1,
           minFreq: GE = 440, maxFreq: GE = 660, ampScale: GE = 0.5f, durScale: GE = 0.5f,
           initCPs: GE = 12, kNum: GE = 12 ) : GE =
     krExp( ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum )
}
case class Gendy1( rate: Rate, ampDist: UGenIn, durDist: UGenIn, adParam: UGenIn, ddParam: UGenIn,
                   minFreq: UGenIn, maxFreq: UGenIn, ampScale: UGenIn, durScale: UGenIn,
                   initCPs: UGenIn, kNum: UGenIn, _indiv: Int )
extends SingleOutUGen( ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale,
                       initCPs, kNum )

object Gendy2 extends UGen12ArgsIndiv {
  def ar( ampDist: GE = 1, durDist: GE = 1, adParam: GE = 1, ddParam: GE = 1,
          minFreq: GE = 440, maxFreq: GE = 660, ampScale: GE = 0.5f, durScale: GE = 0.5f,
          initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31 ) : GE =
    arExp( ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum, a, c )

   def kr( ampDist: GE = 1, durDist: GE = 1, adParam: GE = 1, ddParam: GE = 1,
           minFreq: GE = 440, maxFreq: GE = 660, ampScale: GE = 0.5f, durScale: GE = 0.5f,
           initCPs: GE = 12, kNum: GE = 12, a: GE = 1.17f, c: GE = 0.31 ) : GE =
     krExp( ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale, initCPs, kNum, a, c )
}
case class Gendy2( rate: Rate, ampDist: UGenIn, durDist: UGenIn, adParam: UGenIn, ddParam: UGenIn,
                   minFreq: UGenIn, maxFreq: UGenIn, ampScale: UGenIn, durScale: UGenIn,
                   initCPs: UGenIn, kNum: UGenIn, a: UGenIn, c: UGenIn, _indiv: Int )
extends SingleOutUGen( ampDist, durDist, adParam, ddParam, minFreq, maxFreq, ampScale, durScale,
                       initCPs, kNum, a, c )

object Gendy3 extends UGen9ArgsIndiv {
  def ar( ampDist: GE = 1, durDist: GE = 1, adParam: GE = 1, ddParam: GE = 1,
          freq: GE = 440, ampScale: GE = 0.5f, durScale: GE = 0.5f,
          initCPs: GE = 12, kNum: GE = 12 ) : GE =
    arExp( ampDist, durDist, adParam, ddParam, freq, ampScale, durScale, initCPs, kNum )

   def kr( ampDist: GE = 1, durDist: GE = 1, adParam: GE = 1, ddParam: GE = 1,
           freq: GE = 440, ampScale: GE = 0.5f, durScale: GE = 0.5f,
           initCPs: GE = 12, kNum: GE = 12 ) : GE =
     krExp( ampDist, durDist, adParam, ddParam, freq, ampScale, durScale, initCPs, kNum )
}
case class Gendy3( rate: Rate, ampDist: UGenIn, durDist: UGenIn, adParam: UGenIn, ddParam: UGenIn,
                   freq: UGenIn, ampScale: UGenIn, durScale: UGenIn,
                   initCPs: UGenIn, kNum: UGenIn, _indiv: Int )
extends SingleOutUGen( ampDist, durDist, adParam, ddParam, freq, ampScale, durScale, initCPs, kNum )
