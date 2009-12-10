/*
 *  Buffer.scala
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

import _root_.de.sciss.scalaosc.OSCMessage

/**
 * 	@version	0.13, 24-Nov-09
 */
//object Buffer {
//  
//}

class Buffer( val server: Server = Server.default, val numFrames: Int, val numChannels: Int = 1 )( val bufNum: Int = server.getBufferAllocator.alloc( 1 )) {
//	def this( server: Server = Server.default, numFrames: Int, numChannels: Int = 1 ) = this( server, numFrames, numChannels, server.getBufferAllocator.alloc( 1 ))
//	val bufNum = if( bufNumPreliminary >= 0 ) bufNumPreliminary else server.getBufferAllocator.alloc( 1 )
			
	def free( completionMessage: Option[ Buffer => OSCMessage ] = None ) {
		server.sendMsg( freeMsg( completionMessage ))
	}
 
	def freeMsg( completionMessage: Option[ Buffer => OSCMessage ] = None ) : OSCMessage = {
		uncache
		server.getBufferAllocator.free( bufNum )
		if( completionMessage.isDefined ) {
			OSCMessage( "/b_free", bufNum, completionMessage.get.apply( this ))
		} else {
			OSCMessage( "/b_free", bufNum )
		}
	}
 
	def close( completionMessage: Option[ Buffer => OSCMessage ] = None ) : Unit = {
		server.sendMsg( closeMsg( completionMessage ))
	}
 
	def closeMsg( completionMessage: Option[ Buffer => OSCMessage ] = None ) : OSCMessage = {
		if( completionMessage.isDefined ) {
			OSCMessage( "/b_close", bufNum, completionMessage.get.apply( this ))
		} else {
			OSCMessage( "/b_close", bufNum )
		}
	}
 
	def alloc( completionMessage: Option[ Buffer => OSCMessage ] = None ) : Unit = {
		server.sendMsg( allocMsg( completionMessage ))
	}
 
	def allocMsg( completionMessage: Option[ Buffer => OSCMessage ] = None ) : OSCMessage = {
		cache
		if( completionMessage.isDefined ) {
			OSCMessage( "/b_alloc", bufNum, numFrames, numChannels, completionMessage.get.apply( this ))
		} else {
			OSCMessage( "/b_alloc", bufNum, numFrames, numChannels )
		}
	}

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
