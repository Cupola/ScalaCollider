/*
 *  ControlMappings.scala
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

import collection.immutable.{ IndexedSeq => IIdxSeq }

trait ControlSetMap {
   def toSetSeq: IIdxSeq[ Any ]
   def toSetnSeq: IIdxSeq[ Any ]
}

case class SingleControlSetMap protected[synth]( key: Any, value: Float )
extends ControlSetMap {
   def toSetSeq: IIdxSeq[ Any ]  = Vector( key, value )
   def toSetnSeq: IIdxSeq[ Any ] = Vector( key, 1, value )
}

case class MultiControlSetMap protected[synth]( key: Any, values: IIdxSeq[ Float ])
extends ControlSetMap {
   def toSetSeq: IIdxSeq[ Any ]  = error( "Not yet supported" )
   def toSetnSeq: IIdxSeq[ Any ] = key +: values.size +: values
}

trait ControlBusMap {
//   def toMapSeq: IIdxSeq[ Any ]
   def toMapnSeq: IIdxSeq[ Any ]
}

case class SingleControlBusMap protected[synth]( key: Any, index: Int )
extends ControlBusMap {
   def toMapSeq: IIdxSeq[ Any ]  = Vector( key, index )
   def toMapnSeq: IIdxSeq[ Any ] = Vector( key, index, 1 )
}

case class MultiControlBusMap protected[synth]( key: Any, index: Int, numChannels: Int )
extends ControlBusMap {
   def toMapnSeq: IIdxSeq[ Any ] = Vector( key, index, numChannels )
}
