/*
 *  ContiguousBlockAllocator.scala
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

import collection.immutable.{ Seq => ISeq }
import math._

/**
 * 	@version	0.13, 22-Apr-10
 */
class ContiguousBlockAllocator( size: Int, pos: Int = 0 ) /* extends BlockAllocator */ {
   private val array = new Array[ Block ]( size )
   private var freed = Map[ Int, Set[ Block ]]()
   private var top   = pos
   private val sync  = new AnyRef
  
   // constructor
   {
      array( pos ) = new Block( pos, size - pos )
   }

   def alloc : Int = alloc( 1 )

   def alloc( n: Int ) : Int = {
      sync.synchronized {
         val b = findAvailable( n )
         if( b != null ) reserve( b.start, n, b, null ).start else -1
      }
   }

//   def reserve( address : Int, size : Int = 1 ) : Block = {
//      val b = if( array( address ) != null ) {
//         array( address )
//      } else {
//         findNext( address )
//      }
//
//      if( (b != null) && b.used && (address + size > b.start) ) {
//         error( "The block at (" + address + ", " + size + ") is already in use and cannot be reserved." )
//      }
//
//      if( b.start == address ) {
//         reserve( address, size, b, null )
//      } else {
//         val b2 = findPrevious( address )
//         if( (b2 != null) && b2.used && (b2.start + b2.size > address) ) {
//            error( "The block at (" + address + ", " + size + ") is already in use and cannot be reserved." )
//         }
//         reserve( address, size, null, b2 )
//      }
//   }

   def free( address: Int ) {
      sync.synchronized {
         var b = array( address )
         if( (b != null) && b.used ) {
            b.used = false
            addToFreed( b )
            val prev = findPrevious( address )
            if( (prev != null) && !prev.used ) {
               val temp = prev.join( b )
               if( temp != null ) {
                  if( b.start == top ) {
                     top = temp.start
                  }

                  array( temp.start )	= temp
                  array( b.start )		= null
                  removeFromFreed( prev )
                  removeFromFreed( b )
                  if( top > temp.start ) {
                     addToFreed( temp )
                  }

                  b = temp
               }
            }

            val next = findNext( b.start )
            if( (next != null) && !next.used ) {
               val temp = next.join( b )
               if( temp != null ) {
                  if( next.start == top ) {
                     top = temp.start
                  }

                  array( temp.start ) = temp
                  array( next.start ) = null
                  removeFromFreed( next )
                  removeFromFreed( b )
               }

               if( top > temp.start ) {
                  addToFreed( temp )
               }
            }
         }
      }
   }

//   def allocatedBlocks : ISeq[ Block ] = array.filter( b => (b != null) && b.used )

   private def findAvailable( n: Int ) : Block = {
      freed.get( n ).foreach( set => if( !set.isEmpty ) return set.head )
      freed.foreach( entry => {
    	   if( (entry._1 >= n) && !entry._2.isEmpty ) return entry._2.head
      })

      if( (top + n > size) && array( top ).used ) return null
      array( top )
   }

   private def addToFreed( b: Block ) {
      val setO = freed.get( b.size )
      freed += (b.size -> (if( setO.isDefined ) setO.get + b else Set( b )))
   }
  
   private def removeFromFreed( b: Block ) {
      freed.get( b.size ).foreach( set => {
         val newSet = set - b
         if( newSet.isEmpty ) {
            freed -= b.size
         } else {
            freed += (b.size -> newSet)
         }
      })
   }

   private def findPrevious( address: Int ) : Block = {
      var i = address - 1
      while( i >= pos ) {
         if( array( i ) != null ) return array( i )
         i -= 1
      }
      null
   }

   private def findNext( address: Int ) : Block = {
      val temp = array( address )
      if( temp != null ) return array( temp.start + temp.size )

      var i = address + 1
      while( i <= top ) {
    	   if( array( i ) != null ) return array( i )
    	   i += 1
      }
      null
   }

   private def reserve( address: Int, size: Int, availBlock: Block, prevBlock: Block ) : Block = {
      var b = if( availBlock != null ) availBlock else {
         if( prevBlock != null ) prevBlock else findPrevious( address )
      }

      if( b.start < address) {
         b = split( b, address - b.start, false )._2
      }

      split( b, size, true )._1
   }

   private def split( availBlock: Block, n: Int, used: Boolean ) : Tuple2[Block,Block] = {
      val result		= availBlock.split( n )
      val newB		   = result._1
      val leftOver	= result._2
      newB.used		= used
      removeFromFreed( availBlock )
      if( !used ) addToFreed( newB )

      array( newB.start ) = newB
      if( leftOver != null ) {
         array( leftOver.start ) = leftOver
         if( top > leftOver.start ) {
            addToFreed( leftOver )
         } else {
            top = leftOver.start
         }
      }
	   result
   }
  
   private class Block( val start: Int, val size: Int ) {
      var used = false
    
      def adjoins( b: Block ) : Boolean =
         ((start < b.start) && (start + size >= b.start)) ||
         ((start > b.start) && (b.start + b.size >= start))

      def join( b:Block ) : Block = {
         if( adjoins( b )) {
            val newStart	= min( start, b.start )
            val newSize		= max( start + size, b.start + b.size ) - newStart
            new Block( newStart, newSize )
         } else null
      }

      def split( len: Int ) : Tuple2[ Block, Block ] = {
         if( len < size ) {
            (new Block( start, len ), new Block( start + len, size - len ))
         } else if( len == size ) {
            (this, null)
         } else {
            (null, null)
         }
      }
   }
}