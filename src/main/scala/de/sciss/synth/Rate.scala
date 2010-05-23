/*
 *  Rate.scala
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

package de.sciss.synth

/**
 *    @version 0.11, 23-May-10
 */
object Rate {
   def highest( rates: Rate* ) = rates.foldLeft[ Rate ]( scalar )( (a, b) => if( a.id > b.id ) a else b )
}

/**
 *    The calculation rate of a UGen or a UGen output.
 */
sealed abstract class Rate( val id: Int ) {
   val methodName: String
}

case object scalar  extends Rate( 0 ) { val methodName = "ir" }
case object control extends Rate( 1 ) { val methodName = "kr" }
case object audio   extends Rate( 2 ) { val methodName = "ar" }
case object demand  extends Rate( 3 ) { val methodName = "dr" }

trait RatedGE extends GE {
  def rate : Rate
}

trait ScalarRated  { def rate = scalar }
trait ControlRated { def rate = control }
trait AudioRated   { def rate = audio }
