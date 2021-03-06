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

package de.sciss.synth.osc

import de.sciss.osc.{ OSCException, OSCMessage, OSCPacket, OSCPacketCodec }
import OSCPacket._
import de.sciss.synth.io.{ AudioFileType, SampleFormat }
import java.nio.ByteBuffer
import collection.breakOut
import collection.immutable.{ IndexedSeq => IIdxSeq, Seq => ISeq }
import collection.mutable.ListBuffer
import de.sciss.synth._

/**
 *    @version	0.13, 05-Aug-10
 */
//trait OSCMessageCodec {
//	def decodeMessage( name: String, b: ByteBuffer ) : OSCMessage
//}
//
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

/**
 *    Identifies messages received or sent by the
 *    SuperCollider server
 */
sealed trait OSCServerMessage

/**
 * Identifies messages sent to the SuperCollider server
 */
sealed trait OSCSend extends OSCServerMessage {
   def isSynchronous : Boolean
}
/**
 * Identifies messages sent to the server which are
 * executed synchronously
 */
sealed trait OSCSyncSend extends OSCSend {
   final def isSynchronous = true
}
/**
 * Identifies command messages sent to the server which are
 * executed synchronously and do not return a message
 */
trait OSCSyncCmd extends OSCSyncSend
/**
 * Identifies query messages sent to the server which are
 * executed synchronously and produce a reply message
 */
trait OSCSyncQuery extends OSCSyncSend
/**
 * Identifies messages sent to the server which are
 * executed asynchronously and reply with a form of
 * done-message.
 */
sealed trait OSCAsyncSend extends OSCSend {
   final def isSynchronous = false
}
/**
 * Identifies messages returned by SuperCollider server
 */
trait OSCReceive extends OSCServerMessage

/**
 * Represents a `/synced` message, a reply from the server acknowledging that
 * all asynchronous operations up to the corresponding `/sync` message (i.e. with
 * the same id) have been completed
 */
case class OSCSyncedMessage( id: Int ) extends OSCMessage( "/synced", id )
with OSCReceive

/**
 * Represents a `/sync` message, which is queued with the asynchronous messages
 * on the server, and which, when executed, triggers a corresponding `/synced` reply
 * message (i.e. with the same id)
 *
 * @param   id    an arbitrary identifier which can be used to match the corresponding
 *                reply message. typically the id is incremented by 1 for each
 *                `/sync` message sent out. 
 */
case class OSCSyncMessage( id: Int ) extends OSCMessage( "/sync", id )
with OSCAsyncSend

case class OSCStatusReplyMessage( numUGens: Int, numSynths: Int, numGroups: Int,
                                  numDefs: Int, avgCPU: Float, peakCPU: Float,
                                  sampleRate: Double, actualSampleRate: Double )
extends OSCMessage( "/status.reply", 1, numUGens, numSynths, numGroups, numDefs, avgCPU, peakCPU,
                    sampleRate, actualSampleRate )
with OSCReceive

case object OSCStatusMessage extends OSCMessage( "/status" )
with OSCSyncQuery

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

trait OSCNodeChange extends OSCReceive {
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

// we need List[ Any ] as scala would otherwise expand to List[ Float ]!
case class OSCBufferInfoMessage( infos: OSCBufferInfo* )
extends OSCMessage( "/b_info", infos.flatMap( info =>
   List[ Any ]( info.bufID, info.numFrames, info.numChannels, info.sampleRate )): _* )
with OSCReceive

// ---- messages to the server ----
case class OSCServerNotifyMessage( onOff: Boolean )
extends OSCMessage( "/notify", if( onOff ) 1 else 0 )
with OSCAsyncSend

case object OSCServerQuitMessage extends OSCMessage( "/quit" )
with OSCAsyncSend

case class OSCBufferQueryMessage( ids: Int* ) extends OSCMessage( "/b_query", ids: _* )
with OSCSyncQuery

case class OSCBufferFreeMessage( id: Int, completion: Option[ OSCPacket ])
extends OSCMessage( "/b_free", (completion.map( m => List( id, m )) getOrElse List( id )): _* )
with OSCAsyncSend

case class OSCBufferCloseMessage( id: Int, completion: Option[ OSCPacket ])
extends OSCMessage( "/b_close", (completion.map( m => List( id, m )) getOrElse List( id )): _* )
with OSCAsyncSend

case class OSCBufferAllocMessage( id: Int, numFrames: Int, numChannels: Int, completion: Option[ OSCPacket ])
extends OSCMessage( "/b_alloc", (completion.map( m => List( id, numFrames, numChannels, m ))
                                                   getOrElse List( id, numFrames, numChannels )): _* )
with OSCAsyncSend

case class OSCBufferAllocReadMessage( id: Int, path: String, startFrame: Int, numFrames: Int,
                                      completion: Option[ OSCPacket ])
extends OSCMessage( "/b_allocRead", (completion.map( m => List( id, path, startFrame, numFrames, m ))
                                                       getOrElse List( id, path, startFrame, numFrames )): _* )
with OSCAsyncSend

case class OSCBufferAllocReadChannelMessage( id: Int, path: String, startFrame: Int, numFrames: Int,
                                             channels: List[ Int ], completion: Option[ OSCPacket ])
extends OSCMessage( "/b_allocReadChannel", (List( id, path, startFrame, numFrames ) ::: channels
   ::: completion.map( msg => List( msg )).getOrElse( Nil )): _* )
with OSCAsyncSend

case class OSCBufferReadMessage( id: Int, path: String, fileStartFrame: Int, numFrames: Int, bufStartFrame: Int,
                                 leaveOpen: Boolean, completion: Option[ OSCPacket ])
extends OSCMessage( "/b_read", (completion.map(
   m =>      List( id, path, fileStartFrame, numFrames, bufStartFrame, if( leaveOpen ) 1 else 0, m ))
   getOrElse List( id, path, fileStartFrame, numFrames, bufStartFrame, if( leaveOpen ) 1 else 0 )): _* )
with OSCAsyncSend

case class OSCBufferReadChannelMessage( id: Int, path: String, fileStartFrame: Int, numFrames: Int,
                                        bufStartFrame: Int, leaveOpen: Boolean, channels: List[ Int ],
                                        completion: Option[ OSCPacket ])
extends OSCMessage( "/b_readChannel", (List( id, path, fileStartFrame, numFrames, bufStartFrame,
   if( leaveOpen ) 1 else 0 ) ::: channels ::: completion.map( msg => List( msg )).getOrElse( Nil )): _* )
with OSCAsyncSend

case class OSCBufferZeroMessage( id: Int, completion: Option[ OSCPacket ])
extends OSCMessage( "/b_zero", (completion.map( m => List( id, m )) getOrElse List( id )): _* )
with OSCAsyncSend

case class OSCBufferWriteMessage( id: Int, path: String, fileType: AudioFileType, sampleFormat: SampleFormat,
                                  numFrames: Int, startFrame: Int, leaveOpen: Boolean,
                                  completion: Option[ OSCPacket])
extends OSCMessage( "/b_write", (List( id, path, fileType.id, sampleFormat.id, numFrames, startFrame,
   if( leaveOpen ) 1 else 0 ) ::: completion.map( msg => List( msg )).getOrElse( Nil )): _* )
with OSCAsyncSend

case class OSCBufferSetMessage( id: Int, indicesAndValues: (Int, Float)* )
extends OSCMessage( "/b_set", (id +: indicesAndValues.flatMap( iv => List[ Any ]( iv._1, iv._2 ))): _* )
with OSCSyncCmd

case class OSCBufferSetnMessage( id: Int, indicesAndValues: (Int, IIdxSeq[ Float ])* )
extends OSCMessage( "/b_setn", (id +: indicesAndValues.flatMap( iv => Vector( iv._1, iv._2.size ) ++ iv._2 )): _* )
with OSCSyncCmd

//case class OSCBusValuePair( index: Int, value: Float )
case class OSCControlBusSetMessage( indicesAndValues: (Int, Float)* )
extends OSCMessage( "/c_set", indicesAndValues.flatMap( iv => List[ Any ]( iv._1, iv._2 )): _* )
with OSCSyncCmd

//case class OSCBusValuesPair( index: Int, values: IIdxSeq[ Float ])
case class OSCControlBusSetnMessage( indicesAndValues: (Int, IIdxSeq[ Float ])* )
extends OSCMessage( "/c_setn", indicesAndValues.flatMap( iv => Vector( iv._1, iv._2.size ) ++ iv._2 ): _* )
with OSCSyncCmd

case class OSCControlBusGetMessage( index: Int* ) // fucking hell: indices is defined for SeqLike
extends OSCMessage( "/c_get", index: _* )
with OSCSyncQuery

case class OSCGroupNewInfo( groupID: Int, addAction: Int, targetID: Int )
case class OSCGroupNewMessage( groups: OSCGroupNewInfo* )
extends OSCMessage( "/g_new", groups.flatMap( g => List( g.groupID, g.addAction, g.targetID )): _* )
with OSCSyncCmd

//case class OSCNodeFlagPair( id: Int, flag: Boolean )
case class OSCGroupDumpTreeMessage( groups: (Int, Boolean)* )
extends OSCMessage( "/g_dumpTree", groups.flatMap( g => List( g._1, if( g._2 ) 1 else 0 )): _* )
with OSCSyncCmd

case class OSCGroupQueryTreeMessage( groups: (Int, Boolean)* )
extends OSCMessage( "/g_queryTree", groups.flatMap( g => List( g._1, if( g._2 ) 1 else 0 )): _* )
with OSCSyncQuery

/**
 * Represents an `/g_head` message, which pair-wise places nodes at the head
 * of groups.
 * {{{
 * /g_head
 *   [
 *     Int - the ID of the group at which head a node is to be placed (B)
 *     int - the ID of the node to place (A)
 *   ] * N
 * }}}
 * So that for each pair, node A is moved to the head of group B.
 */
case class OSCGroupHeadMessage( groups: (Int, Int)* )
extends OSCMessage( "/g_head", groups.flatMap( g => List( g._1, g._2 )): _* )
with OSCSyncCmd

/**
 * Represents an `/g_tail` message, which pair-wise places nodes at the tail
 * of groups.
 * {{{
 * /g_tail
 *   [
 *     Int - the ID of the group at which tail a node is to be placed (B)
 *     int - the ID of the node to place (A)
 *   ] * N
 * }}}
 * So that for each pair, node A is moved to the tail of group B.
 */
case class OSCGroupTailMessage( groups: (Int, Int)* )
extends OSCMessage( "/g_tail", groups.flatMap( g => List( g._1, g._2 )): _* )
with OSCSyncCmd

case class OSCGroupFreeAllMessage( ids: Int* )
extends OSCMessage( "/g_freeAll", ids: _* )
with OSCSyncCmd

case class OSCGroupDeepFreeMessage( ids: Int* )
extends OSCMessage( "/g_deepFree", ids: _* )
with OSCSyncCmd

case class OSCSynthNewMessage( defName: String, id: Int, addAction: Int, targetID: Int, controls: ControlSetMap* )
extends OSCMessage( "/s_new",
   (Vector( defName, id, addAction, targetID ) ++ controls.flatMap( _.toSetSeq )): _* )
with OSCSyncCmd

case class OSCNodeRunMessage( nodes: (Int, Boolean)* )
extends OSCMessage( "/n_run", nodes.flatMap( n => List( n._1, if( n._2 ) 1 else 0 )): _* )
with OSCSyncCmd

case class OSCNodeSetMessage( id: Int, pairs: ControlSetMap* )
extends OSCMessage( "/n_set", (id +: pairs.flatMap( _.toSetSeq )): _* )
with OSCSyncCmd

case class OSCNodeSetnMessage( id: Int, pairs: ControlSetMap* )
extends OSCMessage( "/n_setn", (id +: pairs.flatMap( _.toSetnSeq )): _* )
with OSCSyncCmd

case class OSCNodeTraceMessage( ids: Int* )
extends OSCMessage( "/n_trace", ids: _* )
with OSCSyncCmd

case class OSCNodeNoIDMessage( ids: Int* )
extends OSCMessage( "/n_noid", ids: _* )
with OSCSyncCmd

case class OSCNodeFreeMessage( ids: Int* )
extends OSCMessage( "/n_free", ids: _* )
with OSCSyncCmd

case class OSCNodeMapMessage( id: Int, mappings: SingleControlKBusMap* )
extends OSCMessage( "/n_map", (id +: mappings.flatMap( _.toMapSeq )): _* )
with OSCSyncCmd

case class OSCNodeMapnMessage( id: Int, mappings: ControlKBusMap* )
extends OSCMessage( "/n_mapn", (id +: mappings.flatMap( _.toMapnSeq )): _* )
with OSCSyncCmd

case class OSCNodeMapaMessage( id: Int, mappings: SingleControlABusMap* )
extends OSCMessage( "/n_mapa", (id +: mappings.flatMap( _.toMapaSeq )): _* )
with OSCSyncCmd

case class OSCNodeMapanMessage( id: Int, mappings: ControlABusMap* )
extends OSCMessage( "/n_mapan", (id +: mappings.flatMap( _.toMapanSeq )): _* )
with OSCSyncCmd

case class OSCNodeFillInfo( control: Any, numChannels: Int, value: Float )

case class OSCNodeFillMessage( id: Int, fillings: OSCNodeFillInfo* )
extends OSCMessage( "/n_fill", (id +: fillings.flatMap( f => Vector( f.control, f.numChannels, f.value ))): _* )
with OSCSyncCmd

/**
 * Represents an `/n_before` message, which pair-wise places nodes before
 * other nodes.
 * {{{
 * /n_before
 *   [
 *     Int - the ID of the node to place (A)
 *     int - the ID of the node before which the above is placed (B)
 *   ] * N
 * }}}
 * So that for each pair, node A in the same group as node B, to execute immediately before node B.
 */
case class OSCNodeBeforeMessage( groups: (Int, Int)* )
extends OSCMessage( "/n_before", groups.flatMap( g => List( g._1, g._2 )): _* )
with OSCSyncCmd

/**
 * Represents an `/n_after` message, which pair-wise places nodes after
 * other nodes.
 * {{{
 * /n_after
 *   [
 *     Int - the ID of the node to place (A)
 *     int - the ID of the node after which the above is placed (B)
 *   ] * N
 * }}}
 * So that for each pair, node A in the same group as node B, to execute immediately after node B.
 */
case class OSCNodeAfterMessage( groups: (Int, Int)* )
extends OSCMessage( "/n_after", groups.flatMap( g => List( g._1, g._2 )): _* )
with OSCSyncCmd

case class OSCSynthDefRecvMessage( bytes: ByteBuffer, completion: Option[ OSCPacket ])
extends OSCMessage( "/d_recv", (bytes :: (completion.map( List( _ )) getOrElse Nil)): _* )
with OSCAsyncSend

case class OSCSynthDefFreeMessage( names: String* )
extends OSCMessage( "/d_free", names: _* )
with OSCSyncCmd

case class OSCSynthDefLoadMessage( path: String, completion: Option[ OSCPacket ])
extends OSCMessage( "/d_load", (path :: (completion.map( List( _ )) getOrElse Nil)): _* )
with OSCAsyncSend

case class OSCSynthDefLoadDirMessage( path: String, completion: Option[ OSCPacket ])
extends OSCMessage( "/d_loadDir", (path :: (completion.map( List( _ )) getOrElse Nil)): _* )
with OSCAsyncSend
