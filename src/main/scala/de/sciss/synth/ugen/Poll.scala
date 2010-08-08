/*
 *  Poll.scala
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

import de.sciss.synth.{ audio, control, Constant, GE, Rate, SideEffectUGen, SingleOutUGen, SynthGraph, UGenIn }
import SynthGraph._

/**
 *    @version 0.10, 28-Apr-10
 */
object Poll {
   def ar( trig: GE, in: GE, label: String, trigID: GE = -1 ) : GE =
      arExp( trig, in, label, trigID )

   def kr( trig: GE, in: GE, label: String, trigID: GE = -1 ) : GE =
      krExp( trig, in, label, trigID )

   private def make( rate: Rate, trig: GE, in: GE, label: String, trigID: GE ) : GE =
      simplify( for( List( t, i, d ) <- expand( trig, in, trigID ))
         yield this( rate, t, i, if( label != null ) label else {
            val c = label.getClass.getName
            c.substring( c.lastIndexOf( '.' ))
         }, d ))

   private def krExp( trig: GE, in: GE, label: String, trigID: GE ) : GE =
      make( control, trig, in, label, trigID )

   private def arExp( trig: GE, in: GE, label: String, trigID: GE ) : GE =
      make( audio, trig, in, label, trigID )
}
case class Poll( rate: Rate, trig: UGenIn, in: UGenIn, label: String, trigID: UGenIn )
extends SingleOutUGen( (Vector( trig, in, trigID, Constant( label.length )) ++
   label.getBytes( "ISO-8859-1" ).map( Constant( _ ))): _* ) with SideEffectUGen
