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
import ugen.{ AudioControlProxy, ControlProxy, TrigControlProxy }

/**
 *    @version	0.13, 17-May-10
 */
class ControlProxyFactory( name: String ) {
   def ir : GE = ir( Vector( 0f ))
   def ir( value: Double, values: Double* ) : GE = ir( Vector( (value.toFloat +: values.map( _.toFloat )): _* ))
   def ir( value: Float, values: Float* ) : GE = ir( Vector( (value +: values): _* ))
   def kr : GE = kr( Vector( 0f ))
   def kr( value: Double, values: Double* ) : GE = kr( Vector( (value.toFloat +: values.map( _.toFloat )): _* ))
   def kr( value: Float, values: Float* ) : GE = kr( Vector( (value +: values): _* ))
   def tr : GE = tr( Vector( 0f ))
   def tr( value: Double, values: Double* ) : GE = tr( Vector( (value.toFloat +: values.map( _.toFloat )): _* ))
   def tr( value: Float, values: Float* ) : GE = tr( Vector( (value +: values): _* ))
   def ar : GE = ar( Vector( 0f ))
   def ar( value: Double, values: Double* ) : GE = ar( Vector( (value.toFloat +: values.map( _.toFloat )): _* ))
   def ar( value: Float, values: Float* ) : GE = ar( Vector( (value +: values): _* ))
//   def kr[ T <% GE ]( spec: (T, Double), specs: (T, Double)* ) : GE = kr( Vector( (spec._1, spec._2.toFloat) ))
//   def kr[ T <% GE ]( spec: (T, Float), specs: (T, Float)* ) : GE = kr( Vector( spec ))

   @inline private def ir( values: IIdxSeq[ Float ]) : GE  = ControlProxy( scalar, values, Some( name ))
   @inline private def kr( values: IIdxSeq[ Float ]) : GE  = ControlProxy( control, values, Some( name ))
   @inline private def tr( values: IIdxSeq[ Float ]) : GE  = TrigControlProxy( control, values, Some( name ))
   @inline private def ar( values: IIdxSeq[ Float ]) : GE  = AudioControlProxy( control, values, Some( name ))
//   @inline private def kr( specs: IIdxSeq[ (GE, Float) ]) : GE = {
//
//      LagControlProxy( control, values, Some( name ))
//   }

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
