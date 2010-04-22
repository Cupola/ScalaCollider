/*
 *  MixedBundle.scala
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

/*
import _root_.scala.collection.mutable.ListBuffer
import _root_.de.sciss.scalaosc.OSCMessage
  
class MixedBundle {
	private val messages = new ListBuffer[ OSCMessage ]()
	private val preparationMessages = new ListBuffer[ OSCMessage ]()

	def hasPreparationMessages = !preparationMessages.isEmpty
	def hasMessages = !messages.isEmpty
	def isEmpty = preparationMessages.isEmpty && messages.isEmpty
 
	def getPreparationMessages : Seq[ OSCMessage ] = messages.toList
 
	def add( msg: OSCMessage ) { messages += msg }
	def addAll( msgs: Seq[ OSCMessage ]) { messages ++= msgs }
	def addPrepare( msg: OSCMessage ) { preparationMessages += msg }

	// the sound starts at: (next beat + latency) when the preparation is finished
	// the client side task starts at: preparationTime + next beat
	// next beat is 0 if no clock is passed in.
	// eventstreams e.g. take into account the latency internally

//	schedSend { arg server, clock, quant;
//		server = server ?? { Server.default };
//		this.doPrepare(server, {
//			if(quant.isNil or: { clock.isNil }) {
//				this.prSend(server, server.latency)
//			} {
//				clock.schedAbs(quant.nextTimeOnGrid(clock),  {
//					this.prSend(server, server.latency);
//				});
//			};
//		});
//	}
 
	def send( server: Server ) : Unit = send( server, None )

	def send( server: Server, time: Double ) : Unit = send( server, Some( time ))
 
	def send( server: Server, time: Option[ Double ]) {
//		server = server ?? { Server.default };
//		timeOfRequest = timeOfRequest ?? {Main.elapsedTime};
		this.doPrepare( server, () => this.sendDirect( server, time ))
	}

// // see atTime helpfile
//	sendAtTime { arg server, atTime, timeOfRequest; // offset by preparation
//		server = server ?? { Server.default };
//		atTime.schedBundle(this,server,timeOfRequest ?? { Main.elapsedTime});
//	}

	private def doPrepare( server: Server, onComplete: => Unit ) {
		if( preparationMessages.isEmpty ) { onComplete; return }

//		Routine.run {

// XXX

//			server.sync( Condition.new, preparationMessages )
			onComplete
//		};
	}


	// private //

	private def sendDirect( server: Server, delta: Option[ Double ]) {
		if( messages != Nil ) {
			server.sendBundle( delta.getOrElse( server.latency ), messages:_* )
		}
	}
}

*/