/*
 *  BufferManager.scala
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

import scala.collection.immutable.{ IntMap }
import osc._

object BufferManager {
   case class BufferInfo( buffer: Buffer, info: OSCBufferInfo )
}

/**
 *    @version 0.11, 09-May-10
 */
class BufferManager( server: Server ) extends Model {
   import BufferManager._

   private var buffers: IntMap[ Buffer ] = _
   private val sync = new AnyRef

   // ---- constructor ----
   {
      clear
   }

   def bufferInfo( msg: OSCBufferInfoMessage ) {
      sync.synchronized {
         msg.infos.foreach( info => {
            buffers.get( info.bufID ).foreach( buf => {
               // this is the only safe way: automatically unregister,
               // since unlike nodes whose id is steadily increasing
               // and which fire identifiable n_end messages, we
               // would run into trouble. putting unregister in
               // freeMsg like sclang does is not very elegant, as
               // that message might not be sent immediately or not
               // at all.
               buffers -= buf.id
               val change = BufferInfo( buf, info )
               dispatch( change )
               buf.updated( change )
            })
         })
      }
   }

   // eventually this should be done automatically
   // by the message dispatch management
   def register( buf: Buffer ) {
      sync.synchronized { buffers += buf.id -> buf }
   }

   def unregister( buf: Buffer ) {
      sync.synchronized { buffers -= buf.id }
   }

   def clear {
      sync.synchronized {
         buffers = IntMap.empty
      }
//    dispatch( Cleared )
   }
}