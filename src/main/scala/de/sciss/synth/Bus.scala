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
import collection.mutable.{ ListBuffer }
import osc._

class AllocatorExhaustedException( reason: String )
extends RuntimeException( reason )

/**
 *    @version	0.12, 10-May-10
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
	private var released = false

	def free {
	   if( released ) error( this.toString + " : has already been freed" )
		server.busses.freeControl( index )
	   released = true
	}

   def set( v: Float ) {
      server ! setMsg( v )
   }

   def set( pairs: (Int, Float)* ) {
      server ! setMsg( pairs: _* )
   }

   def setn( v: IndexedSeq[ Float ]) {
      server ! setnMsg( v )
   }

   def setn( pairs: (Int, IndexedSeq[ Float ])* ) {
      server ! setnMsg( pairs: _* )
   }

   def setMsg( v: Float ) = {
      require( numChannels == 1 )
      OSCControlBusSetMessage( (index, v) )
   }

   def setMsg( pairs: (Int, Float)* ) = {
      require( pairs.forall( tup => (tup._1 >= 0 && tup._1 < numChannels) ))
      OSCControlBusSetMessage( pairs.map( tup => (tup._1 + index, tup._2) ): _* )
   }

   def setnMsg( v: IndexedSeq[ Float ]) = {
      require( v.size == numChannels )
      OSCControlBusSetnMessage( (index, v.toIndexedSeq) )
   }

   def setnMsg( pairs: (Int, IndexedSeq[ Float ])* ) = {
      require( pairs.forall( tup => (tup._1 >= 0 && (tup._1 + tup._2.size) <= numChannels) ))
      val ipairs = pairs.map( tup => (tup._1 + index, tup._2.toIndexedSeq ))
      OSCControlBusSetnMessage( ipairs: _* )
   }

   def getMsg = {
      require( numChannels == 1 )
      OSCControlBusGetMessage( index )
   }

   def getMsg( offset: Int = 0 ) = {
      require( offset >= 0 && offset < numChannels )
      OSCControlBusGetMessage( offset + index )
   }

   def getMsg( offsets: Int* ) = {
      require( offsets.forall( o => (o >= 0 && o < numChannels) ))
      OSCControlBusGetMessage( offsets.map( _ + index ): _* )
   }
}

case class AudioBus( server: Server, index: Int, numChannels: Int )
extends Bus with AudioRated {
   private var released = false

   def free {
      if( released ) error( this.toString + " : has already been freed" )
      server.busses.freeAudio( index )
      released = true
   }
}