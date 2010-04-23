/*
 *  Bus.scala
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

import collection.immutable.{ IndexedSeq => IIdxSeq }

class AllocatorExhaustedException( reason: String )
extends RuntimeException( reason )

//import de.sciss.tint.sc.{ control => kr, audio => ar } // name conflict

/**
 *    @version	0.12, 18-Jan-10
 */
object Bus {
	def control( server: Server = Server.default, numChannels: Int = 1 ) = {
		val alloc = server.busses.allocControl( numChannels )
		if( alloc == -1 ) {
            throw new AllocatorExhaustedException( "Bus.control: failed to get a bus allocated (" +
				+ numChannels + " channels on " + server.name + ")" )
		}
		ControlBus( server, alloc, numChannels )
	}
  
	def audio( server: Server = Server.default, numChannels: Int = 1 ) = {
		val alloc = server.busses.allocAudio( numChannels )
		if( alloc == -1 ) {
            throw new AllocatorExhaustedException( "Bus.audio: failed to get a bus allocated (" +
				+ numChannels + " channels on " + server.name + ")" )
		}
		AudioBus( server, alloc, numChannels )
	}
}

trait Bus {
   def rate: Rate
   def index: Int
   def numChannels: Int
   def server: Server
   def free: Unit
}

case class ControlBus( server: Server, index: Int, numChannels: Int )
extends Bus with ControlRated {
	private var freed = false

	def free {
	   if( freed ) error( this.toString + " : has already been freed" )
		server.busses.freeControl( index )
	   freed = true
	}

   def set( v: Float ) {
      server ! setMsg( v )
   }

   def set( pairs: Tuple2[ Int, Float ]* ) {
      server ! setMsg( pairs: _* )
   }

   def setn( v: IndexedSeq[ Float ]) {
      server ! setnMsg( v )
   }

   def setn( pairs: Tuple2[ Int, IndexedSeq[ Float ]]* ) {
      server ! setnMsg( pairs: _* )
   }

   def setMsg( v: Float ) = {
      require( numChannels == 1 )
      OSCControlBusSetMessage( (index, v) )
   }

   def setMsg( pairs: Tuple2[ Int, Float ]* ) = {
      require( pairs.forall( tup => (tup._1 >= 0 && tup._1 < numChannels) ))
      OSCControlBusSetMessage( pairs.map( tup => (tup._1 + index, tup._2) ): _* )
   }

   def setnMsg( v: IndexedSeq[ Float ]) = {
      require( v.size == numChannels )
      // XXX replace by toIndexedSeq once it has been fixed to be immutable
      val iv = Vector( v: _* )
      OSCControlBusSetnMessage( (index, iv) )
   }

   def setnMsg( pairs: Tuple2[ Int, IndexedSeq[ Float ]]* ) = {
      require( pairs.forall( tup => (tup._1 >= 0 && (tup._1 + tup._2.size) < numChannels) ))
      // XXX replace by toIndexedSeq once it has been fixed to be immutable
      val ipairs = pairs.map( tup => (tup._1 + index, Vector( tup._2: _* )))
      OSCControlBusSetnMessage( ipairs: _* )
   }
}

case class AudioBus( server: Server, index: Int, numChannels: Int )
extends Bus with AudioRated {
   private var freed = false

   def free {
      if( freed ) error( this.toString + " : has already been freed" )
      server.busses.freeAudio( index )
      freed = true
   }
}