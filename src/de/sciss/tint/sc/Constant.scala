/*
 *  Constant.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2009 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.tint.sc

import java.io.DataOutputStream
import java.io.IOException
import Rates._

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.11, 02-Dec-09
 */
case class Constant( val value: Float ) extends UGenInput {
  val rate = scalar
  
//  def toUGenInputs = List( this )
//  val numOutputs = 1
                             
  def writeInputSpec( dos: DataOutputStream, synthDef: SynthDef ) : Unit = {
      val constIndex = synthDef.getConstantIndex( this )
      if( SynthDef.verbose ) println( "  Constant.writeInputSpec. constIndex = " + constIndex )
      if( constIndex == -1 ) throw new IOException( "Constant not listed in synth def : " + this )
      dos.writeShort( -1 )
      dos.writeShort( constIndex )
  }
}

object Constants {
  val zero		= Constant( 0f )
  val one		= Constant( 1f )
  val minusOne	= Constant( -1f )
}