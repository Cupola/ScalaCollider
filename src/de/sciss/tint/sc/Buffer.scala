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
 * 	@version	0.14, 18-Jan-10
 */
//object Buffer {
//  
//}

class Buffer( val server: Server, val numFrames: Int, val numChannels: Int, val bufNum: Int ) {
    def this( server: Server, numFrames: Int ) =
      this( server, numFrames, 1, server.getBufferAllocator.alloc( 1 ))

    def this( server: Server, numFrames: Int, numChannels: Int ) =
      this( server, numFrames, numChannels, server.getBufferAllocator.alloc( 1 ))

//	def this( server: Server = Server.default, numFrames: Int, numChannels: Int = 1 ) = this( server, numFrames, numChannels, server.getBufferAllocator.alloc( 1 ))
//	val bufNum = if( bufNumPreliminary >= 0 ) bufNumPreliminary else server.getBufferAllocator.alloc( 1 )

    def free { server.sendMsg( freeMsg )}

	def free( completionMessage: OSCMessage ) {
		server.sendMsg( freeMsg( completionMessage ))
	}

	def free( completionMessage: Buffer => OSCMessage ): Unit =
       free( completionMessage.apply( this ))

    def freeMsg: OSCMessage = {
		uncache
		server.getBufferAllocator.free( bufNum )
		OSCMessage( "/b_free", bufNum )
    }

	def freeMsg( completionMessage: OSCMessage ) : OSCMessage = {
		uncache
		server.getBufferAllocator.free( bufNum )
		OSCMessage( "/b_free", bufNum, completionMessage )
	}

	def freeMsg( completionMessage: Buffer => OSCMessage ) : OSCMessage =
       freeMsg( completionMessage.apply( this ))

    def close { server.sendMsg( closeMsg )}

    def close( completionMessage: OSCMessage ) {
		server.sendMsg( closeMsg( completionMessage ))
	}

    def close( completionMessage: Buffer => OSCMessage ): Unit =
       close( completionMessage.apply( this ))
 
	def closeMsg = OSCMessage( "/b_close", bufNum )

	def closeMsg( completionMessage: OSCMessage ) =
      OSCMessage( "/b_close", bufNum, completionMessage )

	def closeMsg( completionMessage: Buffer => OSCMessage ) : OSCMessage =
      closeMsg( completionMessage.apply( this ))
 
	def alloc { server.sendMsg( allocMsg )}

	def alloc( completionMessage: OSCMessage ) {
		server.sendMsg( allocMsg( completionMessage ))
	}
 
	def alloc( completionMessage: Buffer => OSCMessage ): Unit =
      alloc( completionMessage.apply( this ))

	def allocMsg: OSCMessage = {
		cache
		OSCMessage( "/b_alloc", bufNum, numFrames, numChannels )
	}

	def allocMsg( completionMessage: OSCMessage ) : OSCMessage = {
		cache
		OSCMessage( "/b_alloc", bufNum, numFrames, numChannels, completionMessage )
	}

	def allocMsg( completionMessage: Buffer => OSCMessage ) : OSCMessage =
      allocMsg( completionMessage.apply( this ))

 	def cueSoundFileMsg( path: String, startFrame: Int = 0, completionMessage: Option[ Buffer => OSCMessage ] = None ) : OSCMessage = {
		if( completionMessage.isDefined ) {
			OSCMessage( "/b_read", bufNum, path, startFrame, numFrames, 0, 1, completionMessage.get.apply( this ))
		} else {
			OSCMessage( "/b_read", bufNum, path, startFrame, numFrames, 0, 1 )
		}
	}

	// cache Buffers for easy info updating
	private def cache {
//		Buffer.initServerCache(server);
//		serverCaches[server][bufnum] = this;
	}
 
	private def uncache {
//		if(serverCaches[server].notNil,{
//			serverCaches[server].removeAt(bufnum);
//		});
//		if(serverCaches[server].size == 1) {
//			// the 1 item would be the responder
//			// if there is more than 1 item then the rest are cached buffers
//			// else we can remove.
//			// cx: tho i don't see why its important. it will just have to be added
//			// back when the next buffer is added and the responder is removed when
//			// the server reboots
//			Buffer.clearServerCaches(server);
//		}
	}
}