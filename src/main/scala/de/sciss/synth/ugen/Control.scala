/*
 *  Control.scala
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

package de.sciss.synth.ugen

import collection.immutable.{ IndexedSeq => IIdxSeq, Seq => ISeq }
import collection.breakOut
import de.sciss.synth._

/**
 *    @version 0.12, 17-May-10
 */

// ---------- Control ----------

object Control {
   /**
    *    Note: we are not providing further convenience methods,
    *    as that is the task of ControlProxyFactory...
    */
   def ir( values: IIdxSeq[ Float ], name: Option[ String ] = None ) : Control = make( scalar, values, name )
   def kr( values: IIdxSeq[ Float ], name: Option[ String ] = None ) : Control = make( control, values, name )

   def ir( values: Float* ) : Control = ir( Vector( values: _* ))
   def kr( values: Float* ) : Control = kr( Vector( values: _* ))

   private def make( rate: Rate, values: IIdxSeq[ Float ], name: Option[ String ]) : Control = {
      val specialIndex = SynthGraph.builder.addControl( values, name )
      apply( rate, values.size, specialIndex )
   }
}
case class Control private[ugen]( rate: Rate, numChannels: Int, override val specialIndex: Int )
extends MultiOutUGen( rate, numChannels, Nil )

case class ControlProxy( rate: Rate, values: IIdxSeq[ Float ], name: Option[ String ])
extends AbstractControlProxy[ ControlProxy ]( rate, values.size ) {
   def factory = ControlFactory

   override def toString: String = {
      name.getOrElse( "Control" ) + "." + rate.methodName + values.mkString( "(", ", ", ")" )
   }
}

object ControlFactory extends ControlFactoryLike[ ControlProxy ] {
   // XXX eventually we should try to factor this out for all controlfactories...
   def build( proxies: ControlProxy* ) : Map[ ControlProxyLike[ _ ], (UGen, Int) ] = {
      val b = SynthGraph.builder
      proxies.groupBy( _.rate ).flatMap( group => {
         val (rate, ps)    = group
         var numChannels   = 0
         val specialIndex  = ps.map( p => {
            numChannels += p.values.size
            b.addControl( p.values, p.name )
         }).head
         val ugen: UGen = Control( rate, numChannels, specialIndex )
         var offset = 0
         ps.map( p => {
            val res = p -> (ugen, offset)
            offset += p.values.size
            res
         })
      })( breakOut )
   }
}

// ---------- TrigControl ----------

object TrigControl {
   def kr( values: IIdxSeq[ Float ], name: Option[ String ] = None ) : TrigControl = {
      val specialIndex = SynthGraph.builder.addControl( values, name )
      apply( values.size, specialIndex )
   }
   def kr( values: Float* ) : TrigControl = kr( Vector( values: _* ))
}
case class TrigControl private[ugen]( numChannels: Int, override val specialIndex: Int )
extends MultiOutUGen( control, numChannels, Nil ) with ControlRated

case class TrigControlProxy( rate: Rate, values: IIdxSeq[ Float ], name: Option[ String ])
extends AbstractControlProxy[ TrigControlProxy ]( rate, values.size ) {
   def factory = TrigControlFactory

   override def toString: String = {
      name.getOrElse( "TrigControl" ) + "." + rate.methodName + values.mkString( "(", ", ", ")" )
   }
}

object TrigControlFactory extends ControlFactoryLike[ TrigControlProxy ] {
   def build( proxies: TrigControlProxy* ) : Map[ ControlProxyLike[ _ ], (UGen, Int) ] = {
      val b = SynthGraph.builder
      var numChannels   = 0
      val specialIndex  = proxies.map( p => {
         numChannels += p.values.size
         b.addControl( p.values, p.name )
      }).head
      val ugen: UGen = TrigControl( numChannels, specialIndex )
      var offset = 0
      proxies.map( p => {
         val res = p -> (ugen, offset)
         offset += p.values.size
         res
      })( breakOut )
   }
}

// ---------- AudioControl ----------

object AudioControl {
   def ar( values: IIdxSeq[ Float ], name: Option[ String ] = None ) : AudioControl = {
      val specialIndex = SynthGraph.builder.addControl( values, name )
      apply( values.size, specialIndex )
   }
   def ar( values: Float* ) : AudioControl = ar( Vector( values: _* ))
}
case class AudioControl private[ugen]( numChannels: Int, override val specialIndex: Int )
extends MultiOutUGen( audio, numChannels, Nil ) with AudioRated

case class AudioControlProxy( rate: Rate, values: IIdxSeq[ Float ], name: Option[ String ])
extends AbstractControlProxy[ AudioControlProxy ]( rate, values.size ) {
   def factory = AudioControlFactory

   override def toString: String = {
      name.getOrElse( "AudioControl" ) + "." + rate.methodName + values.mkString( "(", ", ", ")" )
   }
}

object AudioControlFactory extends ControlFactoryLike[ AudioControlProxy ] {
   def build( proxies: AudioControlProxy* ) : Map[ ControlProxyLike[ _ ], (UGen, Int) ] = {
      val b = SynthGraph.builder
      var numChannels   = 0
      val specialIndex  = proxies.map( p => {
         numChannels += p.values.size
         b.addControl( p.values, p.name )
      }).head
      val ugen: UGen = AudioControl( numChannels, specialIndex )
      var offset = 0
      proxies.map( p => {
         val res = p -> (ugen, offset)
         offset += p.values.size
         res
      })( breakOut )
   }
}
