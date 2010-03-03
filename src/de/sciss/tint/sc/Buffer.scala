/*
 *  Buffer.scala
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

import de.sciss.scalaosc.OSCMessage

/**
 * 	@version	0.16, 03-Mar-10
 */
object Buffer {
   def alloc( server: Server = Server.default, numFrames: Int, numChannels: Int = 1, completionMessage: Option[ OSCMessage ] = None ) : Buffer = {
      val b = new Buffer( server, numFrames, numChannels )
      b.alloc( completionMessage )
      b
   }

   def read( server: Server = Server.default, path: String, startFrame: Int = 0, numFrames: Int = -1,
             action: Function1[ Buffer, Unit ] = b => () ) : Buffer = {
      val b = new Buffer( server )
//      b.doOnInfo = action
//      b.cache
      b.allocRead( path, startFrame, numFrames, Some( b.queryMsg ))
      b
   }
}

class Buffer private( val id: Int, val server: Server ) extends Model {
   private var numFramesVar   = -1
   private var numChannelsVar = -1
   private var sampleRateVar  = 0f
//   var doOnInfo = (buf: Buffer) => ()

   def numFrames   = numFramesVar
   def numChannels = numChannelsVar
   def sampleRate  = sampleRateVar

   protected[sc] def updated( change: BufferManager.BufferInfo ) {
      val info       = change.info
      numFramesVar   = info.numFrames
      numChannelsVar = info.numChannels
      sampleRateVar  = info.sampleRate
      dispatch( change )
   }

   def this( server: Server, numFrames: Int, numChannels: Int, id: Int ) = {
      this( id, server )
      numFramesVar   = numFrames
      numChannelsVar = numChannels
      sampleRateVar  = server.counts.sampleRate.toFloat
   }

   def this( server: Server ) =
      this( server.bufferAllocator.alloc( 1 ), server )

   def this( server: Server, numFrames: Int ) =
      this( server, numFrames, 1, server.bufferAllocator.alloc( 1 ))

   def this( server: Server, numFrames: Int, numChannels: Int ) =
      this( server, numFrames, numChannels, server.bufferAllocator.alloc( 1 ))


//	def this( server: Server = Server.default, numFrames: Int, numChannels: Int = 1 ) = this( server, numFrames, numChannels, server.getBufferAllocator.alloc( 1 ))
//	val id = if( bufNumPreliminary >= 0 ) bufNumPreliminary else server.getBufferAllocator.alloc( 1 )

   def queryMsg = OSCMessage( "/b_query", id )

   def free { server.sendMsg( freeMsg )}

	def free( completionMessage: Option[ OSCMessage ]) {
		server.sendMsg( freeMsg( completionMessage ))
	}

//	def free( completionMessage: Buffer => OSCMessage ): Unit =
//       free( completionMessage.apply( this ))

   def freeMsg: OSCMessage = {
//      uncache
      server.bufferAllocator.free( id )  // XXX
      OSCMessage( "/b_free", id )
   }

	def freeMsg( completionMessage: Option[ OSCMessage ]) : OSCMessage = {
      completionMessage.map( msg => {
//         uncache
         server.bufferAllocator.free( id )  // XXX
         OSCMessage( "/b_free", id, msg )
      }) getOrElse freeMsg
	}

//	def freeMsg( completionMessage: Buffer => OSCMessage ) : OSCMessage =
//       freeMsg( completionMessage.apply( this ))

   def close { server.sendMsg( closeMsg )}

   def close( completionMessage: Option[ OSCMessage ]) {
      server.sendMsg( closeMsg( completionMessage ))
   }

//    def close( completionMessage: Buffer => OSCMessage ): Unit =
//       close( completionMessage.apply( this ))
 
	def closeMsg = OSCMessage( "/b_close", id )

	def closeMsg( completionMessage: Option[ OSCMessage ]) : OSCMessage = {
      completionMessage.map( msg => {
         OSCMessage( "/b_close", id, msg )
      }) getOrElse closeMsg
   }

//	def closeMsg( completionMessage: Buffer => OSCMessage ) : OSCMessage =
//      closeMsg( completionMessage.apply( this ))
 
	def alloc { server.sendMsg( allocMsg )}

	def alloc( completionMessage: Option[ OSCMessage ]) {
		server.sendMsg( allocMsg( completionMessage ))
	}
 
//	def alloc( completionMessage: Buffer => OSCMessage ): Unit =
//      alloc( completionMessage.apply( this ))

	def allocMsg: OSCMessage = {
//		cache
		OSCMessage( "/b_alloc", id, numFrames, numChannels )
	}

	def allocMsg( completionMessage: Option[ OSCMessage ]) : OSCMessage = {
      completionMessage.map( msg => {
//         cache
         OSCMessage( "/b_alloc", id, numFrames, numChannels, msg )
      }) getOrElse allocMsg
	}

//	def allocMsg( completionMessage: Buffer => OSCMessage ) : OSCMessage =
//      allocMsg( completionMessage.apply( this ))

   def allocRead( path: String, startFrame: Int = 0, numFrames: Int = -1,
                  completionMessage: Option[ OSCMessage ] = None ) {
//      path = argpath;
      server.sendMsg( allocReadMsg( path, startFrame, numFrames, completionMessage ))
   }

   def allocReadMsg( path: String, startFrame: Int = 0, numFrames: Int = -1,
                     completionMessage: Option[ OSCMessage ] = None ) : OSCMessage = {
//      this.cache;
//      path = argpath;
      completionMessage.map( msg => {
                   OSCMessage( "/b_allocRead", id, path, startFrame, numFrames, msg )
      }) getOrElse OSCMessage( "/b_allocRead", id, path, startFrame, numFrames )
   }

   def cueSoundFileMsg( path: String, startFrame: Int = 0, completionMessage: Option[ OSCMessage ] = None ) = {
      completionMessage.map( msg => {
                   OSCMessage( "/b_read", id, path, startFrame, numFrames, 0, 1, msg )
      }) getOrElse OSCMessage( "/b_read", id, path, startFrame, numFrames, 0, 1 )
	}

   def read( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
             leaveOpen: Boolean = false, completionMessage: Option[ OSCMessage ] = None ) {
      server.sendMsg( readMsg( path, fileStartFrame, numFrames, bufStartFrame, leaveOpen, completionMessage ))
   }

   def readMsg( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
                leaveOpen: Boolean = false, completionMessage: Option[ OSCMessage ] = None ) = {
      val loi = if( leaveOpen ) 1 else 0
      completionMessage.map( msg => {
                   OSCMessage( "/b_read", id, path, fileStartFrame, numFrames, bufStartFrame, loi, msg )
      }) getOrElse OSCMessage( "/b_read", id, path, fileStartFrame, numFrames, bufStartFrame, loi )
   }

   def readChannel( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
             leaveOpen: Boolean = false, channels: Seq[ Int ],
             completionMessage: Option[ OSCMessage ] = None ) {
      server.sendMsg( readChannelMsg( path, fileStartFrame, numFrames, bufStartFrame, leaveOpen,
         channels, completionMessage ))
   }

   def readChannelMsg( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
                leaveOpen: Boolean = false, channels: Seq[ Int ],
                completionMessage: Option[ OSCMessage ] = None ) = {
      val loi = if( leaveOpen ) 1 else 0
      val args = List( id, path, fileStartFrame, numFrames, bufStartFrame, loi ) ::: channels.toList :::
         completionMessage.map( msg => List( msg )).getOrElse( Nil )
      OSCMessage( "/b_readChannel", args: _* )
   }

   def zero { server.sendMsg( zeroMsg )}

   def zero( completionMessage: Option[ OSCMessage ]) {
      server.sendMsg( zeroMsg( completionMessage ))
   }

	def zeroMsg = OSCMessage( "/b_zero", id )

	def zeroMsg( completionMessage: Option[ OSCMessage ]) : OSCMessage = {
      completionMessage.map( msg => {
         OSCMessage( "/b_zero", id, msg )
      }) getOrElse zeroMsg
   }

//	// cache Buffers for easy info updating
//	private def cache {
////		Buffer.initServerCache(server);
////		serverCaches[server][id] = this;
//	}
//
//	private def uncache {
////		if(serverCaches[server].notNil,{
////			serverCaches[server].removeAt(id);
////		});
////		if(serverCaches[server].size == 1) {
////			// the 1 item would be the responder
////			// if there is more than 1 item then the rest are cached buffers
////			// else we can remove.
////			// cx: tho i don't see why its important. it will just have to be added
////			// back when the next buffer is added and the responder is removed when
////			// the server reboots
////			Buffer.clearServerCaches(server);
////		}
//	}
}