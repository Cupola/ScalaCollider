/*
 *  DiskIO.scala
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
//import Rates._

/**
 * 	@version	0.10, 19-Jul-09
 */
object DiskOut {	
	def ar( bufNum: GE, channelsArray: GE ) : GE = {
    	UGen.multiNew( "DiskOut", audio, Nil, bufNum :: channelsArray.toUGenIns.toList )
	}
}

object DiskIn {
	def ar( numChannels: Int, bufNum: GE, loop: GE = 0 ) : GE = {
    	UGen.multiNew( "DiskIn", audio, dup( audio, numChannels ), List( bufNum, loop ))
	}

	def ar( buf: Buffer ) : GE = {
		ar( buf, 0 )
	}
	
	def ar( buf: Buffer, loop: GE ) : GE = {
		UGen.multiNew( "DiskIn", audio, dup( audio, buf.numChannels ), List( buf.bufNum, loop ))
	}
}

object VDiskIn {	
	def ar( numChannels: Int, bufNum: GE, rate: GE = 1, loop: GE = 0, sendID: GE = 0 ) : GE = {
    	UGen.multiNew( "VDiskIn", audio, dup( audio, numChannels ), List( bufNum, rate, loop, sendID ))
	}

	// unfortunately we cannot define two ar methods with default args...
	def ar( buf: Buffer ) : GE = {
		ar( buf, 1, 0, 0 )
	}
	
	def ar( buf: Buffer, rate: GE ) : GE = {
		ar( buf, rate, 0, 0 )
	}
	
	def ar( buf: Buffer, rate: GE, loop: GE ) : GE = {
		ar( buf, rate, loop, 0 )
	}
	
	def ar( buf: Buffer, rate: GE, loop: GE, sendID: GE ) : GE = {
		UGen.multiNew( "VDiskIn", audio, dup( audio, buf.numChannels ), List( buf.bufNum, rate, loop, sendID ))
	}
}