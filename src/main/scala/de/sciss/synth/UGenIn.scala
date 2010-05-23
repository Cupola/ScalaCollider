package de.sciss.synth

import collection.immutable.{ IndexedSeq => IIdxSeq }

/**
 *    An element that can be used as an input to a UGen.
 *    This is after multi-channel-expansion, hence implementing
 *    classes are SingleOutUGen, UGenOutProxy, ControlOutProxy, and Constant.
 *
 *    @version 0.11, 23-May-10
 */
trait UGenIn extends RatedGE {
   final override def numOutputs = 1
   final def outputs = Vector( this )
}

/**
 *    A collection of UGenIn objects, wrapped as a graph element.
 *    This is mainly used in multi-channel expansion.
 */
case class UGenInSeq( outputs: IIdxSeq[ UGenIn ]) extends GE {
   override def toString = outputs.mkString( "[", ", ", "]" )

//   override private[synth] def ops = new UGenInSeqOps( this )
}