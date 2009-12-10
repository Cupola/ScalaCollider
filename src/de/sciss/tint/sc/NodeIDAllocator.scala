/*
 *  Constant.scala
 *  NodeIDAllocator
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

import scala.collection.mutable.HashSet

/**
 * 	@author		Hanns Holger Rutz
 *	@version	0.11, 24-Nov-09
 */
class NodeIDAllocator( val user: Int, val initTemp: Int ) {
//  private var initTemp : Int = options.initialNodeID;
    private var temp : Int = initTemp;
	private var mask = user << 26;
	private var perm = 2;
    private var permFreed  = HashSet[ Int ]();
    
    // equivalent to Integer:wrap (_WrapInt)
    private def wrap( x: Int, min: Int, max: Int ) : Int = {
      val width  = max - min;
      val widthp = width + 1;
      val maxp   = max + 1;
      val off	 = x - min;
      val add    = (width - off) / widthp * widthp;
      (off + add) % widthp + min;
    }

    def alloc : Int = {
		var x = temp;
		temp = wrap( x + 1, initTemp, 0x03FFFFFF );
		x | mask;
    }
/*    
    private def reset {
		mask = user << 26;
		temp = initTemp;
		perm = 2;
		permFreed.clear;
    }
*/
}