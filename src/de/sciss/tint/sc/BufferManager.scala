/*
 *  BufferManager.scala
 *  ScalaCollider
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

import scala.collection.immutable.{ IntMap }

object BufferManager {
   case class BufferInfo( buffer: Buffer, info: OSCBufferInfo )
}

class BufferManager( server: Server ) extends Model {
   import BufferManager._

   private var buffers = IntMap.empty[ Buffer ]

   def bufferInfo( msg: OSCBufferInfoMessage ) {
      msg.infos.foreach( info => {
         buffers.get( info.bufID ).foreach( buf => {
            val change = BufferInfo( buf, info )
            dispatch( change )
            buf.updated( change )
         })
      })
   }

   // eventually this should be done automatically
   // by the message dispatch management
   def register( buf: Buffer ) {
      buffers += buf.id -> buf
   }

   def unregister( buf: Buffer ) {
      buffers -= buf.id
   }
}