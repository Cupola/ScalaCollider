package de.sciss.synth

import collection.immutable.{ IndexedSeq => IIdxSeq }

trait SynthGraphBuilder {
   def addUGen( ugen: UGen ) : Unit
   def addControlProxy( proxy: ControlProxyLike[ _ ]) : Unit
   def addControl( values: IIdxSeq[ Float ], name: Option[ String ]) : Int
   def build : SynthGraph

   private var indivCnt = 0
   def individuate : Int = {
      val res = indivCnt
      indivCnt += 1
      res
   }
}
