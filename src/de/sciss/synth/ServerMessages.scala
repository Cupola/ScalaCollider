/*
 *  ServerMessages.scala
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

import de.sciss.scalaosc.{ OSCException, OSCMessage, OSCPacketCodec }
import de.sciss.scalaosc.OSCPacket._
import java.nio.ByteBuffer
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq, Seq => ISeq }
import collection.mutable.{ ListBuffer }

/**
 *    @version	0.11, 22-Apr-10
 */
trait OSCMessageCodec {
	def decodeMessage( name: String, b: ByteBuffer ) : OSCMessage
}

object ServerCodec extends OSCPacketCodec {
	private val decodeStatusReply: (String, ByteBuffer) => OSCMessage = (name, b) => {
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
		
		OSCStatusReplyMessage( numUGens, numSynths, numGroups, numDefs,
		                       avgCPU, peakCPU, sampleRate, actualSampleRate )
	}

   private val decodeSynced: (String, ByteBuffer) => OSCMessage = (name, b) => {
      // ",i"
      if( b.getShort() != 0x2C69 ) decodeFail
      skipToValues( b )

      val id = b.getInt()

      OSCSyncedMessage( id )
   }

	private def decodeNodeChange( factory: OSCNodeMessageFactory ) :
      (String, ByteBuffer) => OSCMessage = (name, b) => {
      
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

      if( nodeType == 0 ) {
         factory.apply( nodeID, OSCSynthInfo( parentID, predID, succID ))
      } else if( (nodeType == 1) && (extTags == 0x6969) ) {	// group
         val headID	= b.getInt()
         val tailID	= b.getInt()
         factory.apply( nodeID, OSCGroupInfo( parentID, predID, succID, headID, tailID ))
		} else decodeFail
	}

   private val decodeBufferInfo: (String, ByteBuffer) => OSCMessage = (name, b) => {
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
      while( cnt > 0 ) {
         infos += OSCBufferInfo( b.getInt(), b.getInt(), b.getInt(), b.getFloat() )
         cnt -= 1
      }
      OSCBufferInfoMessage( infos: _* )
   }

   private val msgDecoders = Map[ String, (String, ByteBuffer) => OSCMessage ](
      "/status.reply"   -> decodeStatusReply,
      "/n_go"			   -> decodeNodeChange( OSCNodeGoMessage ),
      "/n_end"		      -> decodeNodeChange( OSCNodeEndMessage ),
      "/n_off"		      -> decodeNodeChange( OSCNodeOffMessage ),
      "/n_on"			   -> decodeNodeChange( OSCNodeOnMessage ),
      "/n_move"		   -> decodeNodeChange( OSCNodeMoveMessage ),
      "/n_info"		   -> decodeNodeChange( OSCNodeInfoMessage ),
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
}
// val nodeID: Int, val parentID: Int, val predID: Int, val succID: Int, val headID: Int, val tailID: Int )

case class OSCSyncMessage( id: Int ) extends OSCMessage( "/sync", id )
case class OSCSyncedMessage( id: Int ) extends OSCMessage( "/synced", id )

case class OSCStatusReplyMessage( numUGens: Int, numSynths: Int, numGroups: Int,
                                  numDefs: Int, avgCPU: Float, peakCPU: Float,
                                  sampleRate: Double, actualSampleRate: Double )
extends OSCMessage( "/status.reply", 1, numUGens, numSynths, numGroups, numDefs, avgCPU, peakCPU,
                    sampleRate, actualSampleRate )

case object OSCStatusMessage extends OSCMessage( "/status" )

//trait OSCNodeChange {
//	def name: String // aka command (/n_go, /n_end, /n_off, /n_on, /n_move, /n_info)
//	def nodeID:   Int
//	def parentID: Int
//	def predID:   Int
//	def succID:   Int
//}

abstract sealed class OSCNodeInfo {
   def parentID: Int
   def predID:   Int
   def succID:   Int
   def toList( nodeID: Int ): List[ Any ]
}
case class OSCSynthInfo( parentID: Int, predID: Int, succID: Int ) extends OSCNodeInfo {
   def toList( nodeID: Int ) = List( nodeID, parentID, predID, succID, 0 )
}
case class OSCGroupInfo( parentID: Int, predID: Int, succID: Int, headID: Int, tailID: Int ) extends OSCNodeInfo {
   def toList( nodeID: Int ) = List( nodeID, parentID, predID, succID, 1, headID, tailID )
}

trait OSCNodeChange {
   def nodeID: Int
   def info:   OSCNodeInfo
}

protected[synth] trait OSCNodeMessageFactory {
   def apply( nodeID: Int, info: OSCNodeInfo ) : OSCMessage
}

object OSCNodeGoMessage extends OSCNodeMessageFactory
case class OSCNodeGoMessage( nodeID: Int, info: OSCNodeInfo )
extends OSCMessage( "/n_go", info.toList( nodeID ): _* ) with OSCNodeChange

object OSCNodeEndMessage extends OSCNodeMessageFactory
case class OSCNodeEndMessage( nodeID: Int, info: OSCNodeInfo )
extends OSCMessage( "/n_end", info.toList( nodeID ): _* ) with OSCNodeChange

object OSCNodeOnMessage extends OSCNodeMessageFactory
case class OSCNodeOnMessage( nodeID: Int, info: OSCNodeInfo )
extends OSCMessage( "/n_on", info.toList( nodeID ): _* ) with OSCNodeChange

object OSCNodeOffMessage extends OSCNodeMessageFactory
case class OSCNodeOffMessage( nodeID: Int, info: OSCNodeInfo )
extends OSCMessage( "/n_off", info.toList( nodeID ): _* ) with OSCNodeChange

object OSCNodeMoveMessage extends OSCNodeMessageFactory
case class OSCNodeMoveMessage( nodeID: Int, info: OSCNodeInfo )
extends OSCMessage( "/n_move", info.toList( nodeID ): _* ) with OSCNodeChange

object OSCNodeInfoMessage extends OSCNodeMessageFactory
case class OSCNodeInfoMessage( nodeID: Int, info: OSCNodeInfo )
extends OSCMessage( "/n_info", info.toList( nodeID ): _* ) with OSCNodeChange

case class OSCBufferInfo( bufID: Int, numFrames: Int, numChannels: Int, sampleRate: Float )

case class OSCBufferInfoMessage( infos: OSCBufferInfo* )
extends OSCMessage( "/b_info", infos.flatMap( info =>
   List( info.bufID, info.numFrames, info.numChannels, info.sampleRate )): _* )

// ---- messages to the server ----
case class OSCServerNotifyMessage( onOff: Boolean )
extends OSCMessage( "/notify", if( onOff ) 1 else 0 ) with AsyncOSCPacket {
   def replyMessage = OSCMessage( "/done", "/notify" )
}

case object OSCServerQuitMessage extends OSCMessage( "/quit" )

case class OSCBufferQueryMessage( ids: Int* ) extends OSCMessage( "/b_query", ids: _* )

case class OSCBufferFreeMessage( id: Int, completionMessage: Option[ OSCMessage ])
extends OSCMessage( "/b_free", (completionMessage.map( m => List( id, m )) getOrElse List( id )): _* )

case class OSCBufferCloseMessage( id: Int, completionMessage: Option[ OSCMessage ])
extends OSCMessage( "/b_close", (completionMessage.map( m => List( id, m )) getOrElse List( id )): _* )

case class OSCBufferAllocMessage( id: Int, numFrames: Int, numChannels: Int, completionMessage: Option[ OSCMessage ])
extends OSCMessage( "/b_alloc", (completionMessage.map( m => List( id, numFrames, numChannels, m ))
                                                   getOrElse List( id, numFrames, numChannels )): _* )
case class OSCBufferAllocReadMessage( id: Int, path: String, startFrame: Int, numFrames: Int,
                                      completionMessage: Option[ OSCMessage ])
extends OSCMessage( "/b_allocRead", (completionMessage.map( m => List( id, path, startFrame, numFrames, m ))
                                                       getOrElse List( id, path, startFrame, numFrames )): _* )

case class OSCBufferReadMessage( id: Int, path: String, fileStartFrame: Int, numFrames: Int, bufStartFrame: Int,
                                 leaveOpen: Boolean, completionMessage: Option[ OSCMessage ])
extends OSCMessage( "/b_read", (completionMessage.map(
   m =>      List( id, path, fileStartFrame, numFrames, bufStartFrame, if( leaveOpen ) 1 else 0, m ))
   getOrElse List( id, path, fileStartFrame, numFrames, bufStartFrame, if( leaveOpen ) 1 else 0 )): _* )

case class OSCBufferReadChannelMessage( id: Int, path: String, fileStartFrame: Int, numFrames: Int,
                                        bufStartFrame: Int, leaveOpen: Boolean, channels: List[ Int ],
                                        completionMessage: Option[ OSCMessage ])
extends OSCMessage( "/b_readChannel", (List( id, path, fileStartFrame, numFrames, bufStartFrame,
   if( leaveOpen ) 1 else 0 ) ::: channels ::: completionMessage.map( msg => List( msg )).getOrElse( Nil )): _* )

case class OSCBufferZeroMessage( id: Int, completionMessage: Option[ OSCMessage ])
extends OSCMessage( "/b_zero", (completionMessage.map( m => List( id, m )) getOrElse List( id )): _* )

//case class OSCBusValuePair( index: Int, value: Float )
case class OSCControlBusSetMessage( indicesAndValues: Tuple2[ Int, Float ]* )
extends OSCMessage( "/c_set", indicesAndValues.flatMap( iv => List( iv._1, iv._2 )): _* )

//case class OSCBusValuesPair( index: Int, values: IIdxSeq[ Float ])
case class OSCControlBusSetnMessage( indicesAndValues: Tuple2[ Int, IIdxSeq[ Float ]]* )
extends OSCMessage( "/c_setn", indicesAndValues.flatMap( iv => Vector( iv._1, iv._2.size ) ++ iv._2 ): _* )

case class OSCGroupNewInfo( groupID: Int, addAction: Int, targetID: Int )
case class OSCGroupNewMessage( groups: OSCGroupNewInfo* )
extends OSCMessage( "/g_new", groups.flatMap( g => List( g.groupID, g.addAction, g.targetID )): _* )

//case class OSCNodeFlagPair( id: Int, flag: Boolean )
case class OSCGroupDumpTreeMessage( groups: Tuple2[ Int, Boolean ]* )
extends OSCMessage( "/g_dumpTree", groups.flatMap( g => List( g._1, if( g._2 ) 1 else 0 )): _* )

case class OSCGroupQueryTreeMessage( groups: Tuple2[ Int, Boolean ]* )
extends OSCMessage( "/g_queryTree", groups.flatMap( g => List( g._1, if( g._2 ) 1 else 0 )): _* )

case class OSCGroupHeadMessage( groups: Tuple2[ Int, Int ]* )
extends OSCMessage( "/g_head", groups.flatMap( g => List( g._1, g._2 )): _* )

case class OSCGroupTailMessage( groups: Tuple2[ Int, Int ]* )
extends OSCMessage( "/g_tail", groups.flatMap( g => List( g._1, g._2 )): _* )

case class OSCGroupFreeAllMessage( ids: Int* )
extends OSCMessage( "/g_freeAll", ids: _* )

case class OSCGroupDeepFreeMessage( ids: Int* )
extends OSCMessage( "/g_deepFree", ids: _* )

case class OSCSynthNewMessage( defName: String, id: Int, addAction: Int, targetID: Int, controls: ControlSetMap* )
extends OSCMessage( "/s_new",
   (Vector( defName, id, addAction, targetID ) ++ controls.flatMap( _.toSetSeq )): _* )

case class OSCNodeRunMessage( nodes: Tuple2[ Int, Boolean ]* )
extends OSCMessage( "/n_run", nodes.flatMap( n => List( n._1, if( n._2 ) 1 else 0 )): _* )

case class OSCNodeSetMessage( id: Int, pairs: ControlSetMap* )
extends OSCMessage( "/n_set", (id +: pairs.flatMap( _.toSetSeq )): _* )

case class OSCNodeSetnMessage( id: Int, pairs: ControlSetMap* )
extends OSCMessage( "/n_setn", (id +: pairs.flatMap( _.toSetnSeq )): _* )

case class OSCNodeTraceMessage( ids: Int* )
extends OSCMessage( "/n_trace", ids: _* )

case class OSCNodeNoIDMessage( ids: Int* )
extends OSCMessage( "/n_noid", ids: _* )

case class OSCNodeFreeMessage( ids: Int* )
extends OSCMessage( "/n_free", ids: _* )

case class OSCNodeMapMessage( id: Int, mappings: SingleControlBusMap* )
extends OSCMessage( "/n_map", (id +: mappings.flatMap( _.toMapSeq )): _* )

//case class OSCNodeMapInfo( control: Any, index: Int, numChannels: Int )

case class OSCNodeMapnMessage( id: Int, mappings: ControlBusMap* )
extends OSCMessage( "/n_mapn", (id +: mappings.flatMap( _.toMapnSeq )): _* )

case class OSCNodeFillInfo( control: Any, numChannels: Int, value: Float )

case class OSCNodeFillMessage( id: Int, fillings: OSCNodeFillInfo* )
extends OSCMessage( "/n_fill", (id +: fillings.flatMap( f => Vector( f.control, f.numChannels, f.value ))): _* )

case class OSCNodeBeforeMessage( groups: Tuple2[ Int, Int ]* )
extends OSCMessage( "/n_before", groups.flatMap( g => List( g._1, g._2 )): _* )

case class OSCNodeAfterMessage( groups: Tuple2[ Int, Int ]* )
extends OSCMessage( "/n_after", groups.flatMap( g => List( g._1, g._2 )): _* )
