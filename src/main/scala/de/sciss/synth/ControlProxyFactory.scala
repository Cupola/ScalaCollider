/*
 *  ControlProxyFactory.scala
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

import collection.immutable.{ IndexedSeq => IIdxSeq, Seq => ISeq }
import math._
import ugen.ControlProxy

/**
 *    @version	0.13, 17-May-10
 */
class ControlProxyFactory( name: String ) {
   def ir : ControlProxy = ir( Vector( 0f ))
   def ir( value: Double ) : ControlProxy = ir( Vector( value.toFloat ))
   def ir( value: Float ) : ControlProxy = ir( Vector( value ))
   def kr : ControlProxy = kr( Vector( 0f ))
   def kr( value: Double ) : ControlProxy = kr( Vector( value.toFloat ))
   def kr( value: Float ) : ControlProxy = kr( Vector( value ))

   def ir( values: IIdxSeq[ Float ]) = ControlProxy( scalar, values, Some( name ))
   def kr( values: IIdxSeq[ Float ]) = ControlProxy( control, values, Some( name ))

//   def kr( values: (GE, IIdxSeq[ Float ])) : ControlProxy = {
//      val lags = values._1.outputs
//      val inits = values._2
//      val numCh = max( lags.size, inits.size )
//      new ControlProxy( Some( name ), control, wrapExtend( values._2, numCh ), Some( wrapExtend( lags, numCh )))
//   }
  
//   private def wrapExtend[T]( coll: IIdxSeq[T], size: Int ) : IIdxSeq[T] = {
//      if( coll.size == size ) coll
//      else if( coll.size > size ) coll.take( size )
//      else {
//         var result = coll
//         while( result.size < size ) {
//            val diff = size - result.size
//            result ++= (if( diff >= coll.size ) coll else coll.take( diff ))
//         }
//         result
//      }
//   }
}

trait ControlFactoryLike[ T ] {
   type Proxy = T // don't ask me what this is doing. some vital variance correction...
   def build( proxies: Proxy* ) : Map[ ControlProxyLike[ _ ], (UGen, Int) ]
}

trait ControlProxyLike[ Impl ] extends RatedGE {
   def factory: ControlFactoryLike[ Impl ]
}

abstract class AbstractControlProxy[ Impl ]( outputRates: IIdxSeq[ Rate ])
extends ControlProxyLike[ Impl ] {
   // ---- constructor ----
   {
      SynthGraph.builder.addControlProxy( this )
   }

   def this( rate: Rate, numOutputs: Int ) =  this( Vector.fill( numOutputs )( rate ))

   final override def numOutputs = outputRates.size
	final def outputs: IIdxSeq[ UGenIn ] = outputRates.zipWithIndex.map(
      tup => ControlOutProxy( this, tup._2, tup._1 ))
}

case class ControlOutProxy( source: ControlProxyLike[ _ ], outputIndex: Int, rate: Rate )
extends UGenIn