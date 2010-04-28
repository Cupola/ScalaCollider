/*
 *  AudioFile.scala
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

object AudioFile {
   abstract sealed class Type( val id: String )
   case object AIFF     extends Type( "aiff" )
   case object NeXT     extends Type( "next" )
   case object Wave     extends Type( "wav" )
   case object IRCAM    extends Type( "ircam")
   case object Raw      extends Type( "raw" )
   case object Wave64   extends Type( "w64" )

   abstract sealed class SampleFormat( val id: String )
   case object Int8     extends SampleFormat( "int8" )
   case object Int16    extends SampleFormat( "int16" )
   case object Int24    extends SampleFormat( "int24" )
   case object Int32    extends SampleFormat( "int32" )
   case object Float    extends SampleFormat( "float" )
   case object Double   extends SampleFormat( "Double" )
   case object MuLaw    extends SampleFormat( "mulaw" )
   case object ALaw     extends SampleFormat( "alaw" )
}