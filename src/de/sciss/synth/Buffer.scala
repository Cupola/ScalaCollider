/*
 *  Buffer.scala
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

import de.sciss.scalaosc.OSCMessage
import ugen.{ BufRateScale, FreeSelfWhenDone, PlayBuf }

/**
 * 	@version	0.17, 28-Apr-10
 */
object Buffer {
   def alloc( server: Server = Server.default, numFrames: Int, numChannels: Int = 1, completionMessage: Option[ OSCMessage ] = None ) : Buffer = {
      val b = new Buffer( server, numFrames, numChannels )
      b.alloc( completionMessage )
      b
   }

   def read( server: Server = Server.default, path: String, startFrame: Int = 0, numFrames: Int = -1,
             action: Buffer => Unit = _ => () ) : Buffer = {
      val b = new Buffer( server )
      b.register
      lazy val l: (AnyRef) => Unit = _ match {
         case BufferManager.BufferInfo( _, _ ) => {
            b.removeListener( l )
            action( b )
         }
      }
      b.addListener( l )
//      b.doOnInfo = action
//      b.cache
      b.allocRead( path, startFrame, numFrames, Some( b.queryMsg ))
      b
   }

   def readChannel( server: Server = Server.default, path: String, startFrame: Int = 0, numFrames: Int = -1,
                    channels: Seq[ Int ], action: Buffer => Unit = _ => () ) : Buffer = {
      val b = new Buffer( server )
      b.register
      lazy val l: (AnyRef) => Unit = _ match {
         case BufferManager.BufferInfo( _, _ ) => {
            b.removeListener( l )
            action( b )
         }
      }
      b.addListener( l )
//      b.doOnInfo = action
//      b.cache
      b.allocReadChannel( path, startFrame, numFrames, channels, Some( b.queryMsg ))
      b
   }
}

class Buffer private( val id: Int, val server: Server ) extends Model {
   private var numFramesVar   = -1
   private var numChannelsVar = -1
   private var sampleRateVar  = 0f
//   var doOnInfo = (buf: Buffer) => ()

   def this( server: Server, numFrames: Int, numChannels: Int, id: Int ) = {
      this( id, server )
      numFramesVar   = numFrames
      numChannelsVar = numChannels
      sampleRateVar  = server.counts.sampleRate.toFloat
   }

   def this( server: Server ) =
      this( server.buffers.alloc( 1 ), server )

   def this( server: Server, numFrames: Int ) =
      this( server, numFrames, 1, server.buffers.alloc( 1 ))

   def this( server: Server, numFrames: Int, numChannels: Int ) =
      this( server, numFrames, numChannels, server.buffers.alloc( 1 ))

   def numFrames   = numFramesVar
   def numChannels = numChannelsVar
   def sampleRate  = sampleRateVar

   def register {
//  	NodeWatcher.register( this, assumePlaying )
       server.bufMgr.register( this )
   }

   protected[synth] def updated( change: BufferManager.BufferInfo ) {
      val info       = change.info
      numFramesVar   = info.numFrames
      numChannelsVar = info.numChannels
      sampleRateVar  = info.sampleRate
      dispatch( change )
   }


//	def this( server: Server = Server.default, numFrames: Int, numChannels: Int = 1 ) = this( server, numFrames, numChannels, server.getBufferAllocator.alloc( 1 ))
//	val id = if( bufNumPreliminary >= 0 ) bufNumPreliminary else server.getBufferAllocator.alloc( 1 )

   def queryMsg = OSCBufferQueryMessage( id )

   def free { server ! freeMsg }

	def free( completionMessage: Option[ OSCMessage ]) {
		server ! freeMsg( completionMessage )
	}

//	def free( completionMessage: Buffer => OSCMessage ): Unit =
//       free( completionMessage.apply( this ))

   def freeMsg: OSCBufferFreeMessage = freeMsg( None )

	def freeMsg( completionMessage: Option[ OSCMessage ]) = {
      server.buffers.free( id )  // XXX
      OSCBufferFreeMessage( id, completionMessage )
	}

//	def freeMsg( completionMessage: Buffer => OSCMessage ) : OSCMessage =
//       freeMsg( completionMessage.apply( this ))

   def close { server ! closeMsg }

   def close( completionMessage: Option[ OSCMessage ]) {
      server ! closeMsg( completionMessage )
   }

//    def close( completionMessage: Buffer => OSCMessage ): Unit =
//       close( completionMessage.apply( this ))
 
	def closeMsg: OSCBufferCloseMessage = closeMsg( None )

	def closeMsg( completionMessage: Option[ OSCMessage ]) =
      OSCBufferCloseMessage( id, completionMessage )

	def alloc { server ! allocMsg }

	def alloc( completionMessage: Option[ OSCMessage ]) {
		server ! allocMsg( completionMessage )
	}
 
	def allocMsg: OSCBufferAllocMessage = allocMsg( None )

	def allocMsg( completionMessage: Option[ OSCMessage ]) =
      OSCBufferAllocMessage( id, numFrames, numChannels, completionMessage )

   def allocRead( path: String, startFrame: Int = 0, numFrames: Int = -1,
                  completionMessage: Option[ OSCMessage ] = None ) {
//      path = argpath;
      server ! allocReadMsg( path, startFrame, numFrames, completionMessage )
   }

   def allocReadMsg( path: String, startFrame: Int = 0, numFrames: Int = -1,
                     completionMessage: Option[ OSCMessage ] = None ) = {
//      this.cache;
//      path = argpath;
      OSCBufferAllocReadMessage( id, path, startFrame, numFrames, completionMessage )
   }

   def allocReadChannel( path: String, startFrame: Int = 0, numFrames: Int = -1, channels: Seq[ Int ],
                         completionMessage: Option[ OSCMessage ] = None ) {
//      path = argpath;
      server ! allocReadChannelMsg( path, startFrame, numFrames, channels, completionMessage )
   }

   def allocReadChannelMsg( path: String, startFrame: Int = 0, numFrames: Int = -1, channels: Seq[ Int ],
                            completionMessage: Option[ OSCMessage ] = None ) = {
//      this.cache;
//      path = argpath;
      OSCBufferAllocReadChannelMessage( id, path, startFrame, numFrames, channels.toList, completionMessage )
   }

   def cueSoundFileMsg( path: String, startFrame: Int = 0, completionMessage: Option[ OSCMessage ] = None ) =
      OSCBufferReadMessage( id, path, startFrame, numFrames, 0, true, completionMessage )

   def read( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
             leaveOpen: Boolean = false, completionMessage: Option[ OSCMessage ] = None ) {
      server ! readMsg( path, fileStartFrame, numFrames, bufStartFrame, leaveOpen, completionMessage )
   }

   def readMsg( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
                leaveOpen: Boolean = false, completionMessage: Option[ OSCMessage ] = None ) =
      OSCBufferReadMessage( id, path, fileStartFrame, numFrames, bufStartFrame, leaveOpen, completionMessage )

   def readChannel( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
             leaveOpen: Boolean = false, channels: Seq[ Int ],
             completionMessage: Option[ OSCMessage ] = None ) {
      server ! readChannelMsg( path, fileStartFrame, numFrames, bufStartFrame, leaveOpen,
         channels, completionMessage )
   }

   def readChannelMsg( path: String, fileStartFrame: Int = 0, numFrames: Int = -1, bufStartFrame: Int = 0,
                leaveOpen: Boolean = false, channels: Seq[ Int ],
                completionMessage: Option[ OSCMessage ] = None ) =
      OSCBufferReadChannelMessage( id, path, fileStartFrame, numFrames, bufStartFrame, leaveOpen, channels.toList,
         completionMessage )

   def zero { server ! zeroMsg }

   def zero( completionMessage: Option[ OSCMessage ]) {
      server ! zeroMsg( completionMessage )
   }

	def zeroMsg: OSCBufferZeroMessage = zeroMsg( None )

	def zeroMsg( completionMessage: Option[ OSCMessage ]) =
      OSCBufferZeroMessage( id, completionMessage )

   // ---- utility methods ----
   def play( loop: Boolean = false, amp: Float = 1f, out: Int = 0 ) : Synth = {
      import SC._
      SC.play( server, out ) { // working around nasty compiler bug
         val ply = PlayBuf.ar( numChannels, id, BufRateScale.kr( id ), loop = if( loop ) 1 else 0 )
         if( !loop ) FreeSelfWhenDone.kr( ply )
         ply * "amp".kr( amp )
      }
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