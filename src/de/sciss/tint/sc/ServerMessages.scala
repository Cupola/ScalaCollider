/*
 *  ServerMessages.scala
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

import de.sciss.scalaosc.{ OSCException, OSCMessage, OSCPacketCodec }
import de.sciss.scalaosc.OSCPacket._
import java.nio.ByteBuffer
import scala.collection.mutable.{ ListBuffer }

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.10, 27-Nov-09
 */
trait OSCMessageCodec {
	def decodeMessage( name: String, b: ByteBuffer ) : OSCMessage
}

object ServerCodec extends OSCPacketCodec {
	private val msgDecoders = Map[ String, (String, ByteBuffer) => OSCMessage ](
		"/status.reply"   -> decodeStatusReply,
		"/n_go"			   -> decodeNodeChange,
		"/n_end"		      -> decodeNodeChange,
		"/n_off"		      -> decodeNodeChange,
		"/n_on"			   -> decodeNodeChange,
		"/n_move"		   -> decodeNodeChange,
		"/n_info"		   -> decodeNodeChange,
      "/synced"         -> decodeSynced,
      "/b_info"         -> decodeBufferInfo,
		"status.reply"	   -> decodeStatusReply
	)

    private val superDecoder: (String, ByteBuffer ) => OSCMessage =
      (name, b) => super.decodeMessage( name, b )

	override protected def decodeMessage( name: String, b: ByteBuffer ) : OSCMessage = {
        msgDecoders.getOrElse( name, superDecoder ).apply( name, b )
/*		val dec = msgDecoders.get( name )
		if( dec.isDefined ) {
			dec.get.apply( name, b )
		} else {
			super.decodeMessage( name, b )
		}
*/	}
	
	private def decodeFail : Nothing = throw new OSCException( OSCException.DECODE, null )
	
	private def decodeStatusReply( name: String, b: ByteBuffer ) : OSCMessage = {
		// ",iiiiiffdd"
		if( (b.getLong != 0x2C69696969696666L) || (b.getShort != 0x6464) ) decodeFail
		skipToValues( b )
		
//		if( b.getInt() != 1) decodeFail  // was probably intended as a version number...
		b.getInt()
		val numUGens		= b.getInt()
		val numSynths		= b.getInt()
		val numGroups		= b.getInt()
		val numDefs			= b.getInt()
		val avgCPU			= b.getFloat()
		val peakCPU			= b.getFloat()
		val sampleRate		= b.getDouble()
		val actualSampleRate= b.getDouble()
		
		new OSCStatusReplyMessage( numUGens, numSynths, numGroups, numDefs,
		                           avgCPU, peakCPU, sampleRate, actualSampleRate )
	}

   private def decodeSynced( name: String, b: ByteBuffer ) : OSCMessage = {
      // ",i"
      if( b.getShort() != 0x2C69 ) decodeFail
      skipToValues( b )

      val id = b.getInt()

      new OSCSyncedMessage( id )
   }

	private def decodeNodeChange( name: String, b: ByteBuffer ) : OSCMessage = {
		// ",iiiii[ii]"
		if( (b.getInt() != 0x2C696969) || (b.getShort() != 0x6969) ) decodeFail
		val extTags = b.getShort()
		if( (extTags & 0xFF) == 0x00 ) {
			skipToAlign( b )
		} else {
			skipToValues( b )
		}
		val nodeID		= b.getInt()
		val parentID	= b.getInt()
		val predID		= b.getInt()
		val succID		= b.getInt()
		val nodeType	= b.getInt()
		
		if( nodeType == 0 ) {	// synth
			new OSCSynthChangeMessage( name, nodeID, parentID, predID, succID )
		} else if( (nodeType == 1) && (extTags == 0x6969) ) {	// group
			val headID	= b.getInt()
			val tailID	= b.getInt()
			new OSCGroupChangeMessage( name, nodeID, parentID, predID, succID, headID, tailID )
		} else decodeFail
	}

   private def decodeBufferInfo( name: String, b: ByteBuffer ) : OSCMessage = {
      // ",[iiif]*N"
      if( b.get() != 0x2C ) decodeFail
      var cnt = 0
      var tag = b.getShort()
      while( tag != 0x0000 ) {
         if( (tag != 0x6969) || (b.getShort() != 0x6966) ) decodeFail
         cnt += 1
         tag = b.getShort()
      }
      skipToAlign( b )
      var infos = new ListBuffer[ OSCBufferInfo ]
      var i = 0; while( i < cnt ) {
         infos += OSCBufferInfo( b.getInt(), b.getInt(), b.getInt(), b.getFloat() )
      }
      new OSCBufferInfoMessage( infos: _* )
   }
}
// val nodeID: Int, val parentID: Int, val predID: Int, val succID: Int, val headID: Int, val tailID: Int )

class OSCSyncMessage( val id: Int ) extends OSCMessage( "/sync", id )
class OSCSyncedMessage( val id: Int ) extends OSCMessage( "/synced", id )

class OSCStatusReplyMessage( val numUGens: Int, val numSynths: Int, val numGroups: Int,
                             val numDefs: Int, val avgCPU: Float, val peakCPU: Float,
                             val sampleRate: Double, val actualSampleRate: Double )
extends OSCMessage( "/status.reply", 1, numUGens, numSynths, numGroups, numDefs, avgCPU, peakCPU, sampleRate, actualSampleRate )

object OSCStatusMessage extends OSCMessage( "/status" )

trait OSCNodeChange {
	def name: String // aka command (/n_go, /n_end, /n_off, /n_on, /n_move, /n_info)
	def nodeID:   Int
	def parentID: Int
	def predID:   Int
	def succID:   Int
}

trait OSCSynthChange extends OSCNodeChange

trait OSCGroupChange extends OSCNodeChange {
	def headID: Int
	def tailID: Int
}

class OSCSynthChangeMessage( name: String, val nodeID: Int, val parentID: Int, val predID: Int, val succID: Int )
extends OSCMessage( name, nodeID, parentID, predID, succID, 0 ) with OSCSynthChange

class OSCGroupChangeMessage( name: String, val nodeID: Int, val parentID: Int, val predID: Int, val succID: Int, val headID: Int, val tailID: Int )
extends OSCMessage( name, nodeID, parentID, predID, succID, 1, headID, tailID ) with OSCGroupChange

case class OSCBufferInfo( bufID: Int, numFrames: Int, numChannels: Int, sampleRate: Float )

class OSCBufferInfoMessage( val infos: OSCBufferInfo* )
extends OSCMessage( "/b_info", infos.flatMap( info =>
   List( info.bufID, info.numFrames, info.numChannels, info.sampleRate )): _* )