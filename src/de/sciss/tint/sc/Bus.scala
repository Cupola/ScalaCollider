/*
 *  Bus.scala
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
package de.sciss.tint.sc

class AllocatorExhaustedException( reason: String )
extends RuntimeException( reason )

import de.sciss.tint.sc.{ control => kr, audio => ar } // name conflict

/**
 *	@version	0.12, 18-Jan-10
 */
object Bus {
	def control( server: Server = Server.default, numChannels: Int = 1 ) : Bus = {
		val alloc = server.busses.allocControl( numChannels )
		if( alloc == -1 ) {
            throw new AllocatorExhaustedException( "Bus.control: failed to get a bus allocated (" +
				+ numChannels + " channels on " + server.name + ")" )
		}
		new Bus( kr, alloc, numChannels, server )
	}
  
	def audio( server: Server = Server.default, numChannels: Int = 1 ) : Bus = {
		val alloc = server.busses.allocAudio( numChannels )
		if( alloc == -1 ) {
            throw new AllocatorExhaustedException( "Bus.audio: failed to get a bus allocated (" +
				+ numChannels + " channels on " + server.name + ")" )
		}
		new Bus( ar, alloc, numChannels, server )
	}
}

case class Bus( rate: Rate, index: Int, numChannels: Int, server: Server ) {
	private var freed = false

	def free {
	  if( freed ) {
	     println( "WARNING: Bus.free : has already been freed" )
	     return
	  }

	  if( rate == audio ) {
		  server.busses.freeAudio( index )
	  } else {
		  server.busses.freeControl( index )
	  }
	  freed = true
	}
}
