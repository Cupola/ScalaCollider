/*
 *  GraphFunction.scala
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

import de.sciss.osc.OSCMessage

object GraphFunction {
   private var uniqueIDCnt = 0
   private val uniqueSync = new AnyRef
   private def uniqueID = {
      uniqueSync.synchronized {
         uniqueIDCnt += 1
         val result = uniqueIDCnt
         result
      }
   }
}

class GraphFunction[ T <% GE ]( thunk: => T ) {
   import GraphFunction._
   
   def play : Synth = {
      play( Server.default.defaultGroup, 0, Some(0.02f), addToHead )
   }

   def play( target: Node = Server.default.defaultGroup, outBus: Int = 0,
             fadeTime: Option[Float] = Some( 0.02f ),
             addAction: AddAction = addToHead ) : Synth = {

		val server = target.server
		val defName    = "temp_" + uniqueID // more clear than using hashCode
		val synthDef   = SynthDef( defName, SynthGraph.wrapOut( thunk, fadeTime ))
		val synth      = new Synth( server )
		val bytes      = synthDef.toBytes
		val synthMsg   = synth.newMsg( synthDef.name, target, List( "i_out" -> outBus, "out" -> outBus ), addAction )
      synth.onEnd { server ! synthDef.freeMsg } // why would we want to accumulate the defs?
		if( bytes.remaining > (65535 / 4) ) { // "preliminary fix until full size works" (???)
			if( server.isLocal ) {
				synthDef.load( server, completion = synthMsg )
			} else {
				warn( "synthdef may have been too large to send to remote server" )
				server ! OSCMessage( "/d_recv", bytes, synthMsg )
			}
		} else {
			server ! OSCMessage( "/d_recv", bytes, synthMsg )
		}
		synth
	}
}