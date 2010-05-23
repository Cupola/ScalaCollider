/*
 *  UGenIn.scala
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

/**
 *    An element that can be used as an input to a UGen.
 *    This is after multi-channel-expansion, hence implementing
 *    classes are SingleOutUGen, UGenOutProxy, ControlOutProxy, and Constant.
 *
 *    @version 0.11, 23-May-10
 */
sealed trait UGenIn extends RatedGE {
   final override def numOutputs = 1
   final def outputs = Vector( this )
}

/**
 *    A scalar constant used as an input to a UGen.
 *    These constants are stored in a separate table of
 *    the synth graph.
 */
case class Constant( value: Float ) extends UGenIn with ScalarRated {
   override def toString = value.toString

   override private[synth] def ops = new ConstantOps( this )

   // special binop handling
   override def +( b: GE ) : GE        = b match {
      case Constant( bval ) => Constant( value + bval )
      case _ => super.+( b )
   }
}

/**
 *    A SingleOutUGen is a UGen which has exactly one output, and
 *    hence can directly function as input to another UGen without expansion.
 */
abstract class SingleOutUGen( val inputs: UGenIn* ) extends UGen with UGenIn

/**
 *    A UGenOutProxy refers to a particular output of a multi-channel UGen.
 *    A sequence of these form the representation of a multi-channel-expanded
 *    UGen. 
 */
case class UGenOutProxy( source: UGen, outputIndex: Int, rate: Rate )
extends UGenIn with UGenProxy {
   override def toString = "(" + source + " \\ " + outputIndex + ")"
}

/**
 *    A ControlOutProxy is similar to a UGenOutProxy in that it denotes
 *    an output channel of a control UGen. However it refers to a control-proxy
 *    instead of a real control ugen, since the proxies are synthesized into
 *    actual ugens only at the end of a synth graph creation, in order to
 *    clumb several controls together. ControlOutProxy instance are typically
 *    returned from the ControlProxyFactory class, that is, using the package
 *    implicits, from calls such as "myControl".kr.
 */
case class ControlOutProxy( source: ControlProxyLike[ _ ], outputIndex: Int, rate: Rate )
extends UGenIn {
   override def toString = "(" + source + " \\ " + outputIndex + ")"
}