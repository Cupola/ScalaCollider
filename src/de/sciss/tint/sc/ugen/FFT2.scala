/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._
import SC._
import GraphBuilder._

object RunningSum extends UGen2Args {
  def ar( in: GE, numSamples: GE = 400 ) : GE = arExp( in, numSamples )
  def kr( in: GE, numSamples: GE = 400 ) : GE = krExp( in, numSamples )

  def rms( in: GE, numSamples: GE = 400 ) : GE =
    (ar( in.squared, numSamples ) * numSamples.reciprocal).sqrt
}
case class RunningSum( rate: Rate, in: UGenIn, numSamples: UGenIn )
extends SingleOutUGen( in, numSamples )