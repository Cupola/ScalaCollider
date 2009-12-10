/*
 *  Bus.scala
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

/**
 *	@version	0.11, 16-Jun-09
 */
object Bus {
	def control( server: Server = Server.default, numChannels: Int = 1 ) : Bus = {
		val alloc = server.busses.allocControl( numChannels )
		if( alloc == -1 ) {
			println( "Meta_Bus:control: failed to get a control bus allocated."
				+ "numChannels:" + numChannels + "server:" + server.name )
			return null
		}
		new Bus( 'control, alloc, numChannels, server )
	}
  
	def audio( server: Server = Server.default, numChannels: Int = 1 ) : Bus = {
		val alloc = server.busses.allocAudio( numChannels )
		if( alloc == -1 ) {
			println("Meta_Bus:audio: failed to get an audio bus allocated."
				+ "numChannels:" + numChannels + "server:" + server.name )
			return null
		}
		new Bus( 'audio, alloc, numChannels, server )
	}
}

case class Bus( rate: Symbol, index: Int, numChannels: Int, server: Server ) {
	private var freed = false

	def free {
	  if( freed ) {
	     println( "WARNING: Bus.free : has already been freed" )
	     return
	  }

	  if( rate == 'audio ) {
		  server.busses.freeAudio( index )
	  } else {
		  server.busses.freeControl( index )
	  }
	  freed = true
	}
}
