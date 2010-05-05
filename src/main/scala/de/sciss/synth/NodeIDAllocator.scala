/*
 *  NodeIDAllocator.scala
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

/**
 *    @version	0.12, 22-Apr-10
 */
class NodeIDAllocator( user: Int, initTemp: Int ) {
   private var temp  = initTemp
   private val mask  = user << 26
//   private val perm = 2
//   private var permFreed = HashSet[ Int ]();
   private val sync = new AnyRef
    
   // equivalent to Integer:wrap (_WrapInt)
   private def wrap( x: Int, min: Int, max: Int ) : Int = {
      val width  = max - min
      val widthp = width + 1
      val maxp   = max + 1
      val off	  = x - min
      val add    = (width - off) / widthp * widthp
      (off + add) % widthp + min
   }

   def alloc : Int = {
      sync.synchronized {
         val x = temp
         temp = wrap( x + 1, initTemp, 0x03FFFFFF )
         x | mask
      }
   }
}