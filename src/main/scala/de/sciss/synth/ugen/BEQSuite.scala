/*
 *  BEQSuite.scala
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

import de.sciss.synth.{ AudioRated, GE, SingleOutUGen, UGenIn }

object BLowPass extends UGen3RArgs {
   def ar( in: GE, freq: GE = 1200, rq: GE = 1 ) : GE =
      make( in, freq, rq )
}
/**
 * A 2nd order (12db per oct rolloff) resonant low pass filter UGen.
 * The B equalization suite is based on the Second Order Section (SOS) biquad UGen.
 *
 * Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
 * plugin UGens contain optimizations such that the coefficients get updated only when
 * there has been a change to one of the filter's parameters. This can cause spikes in
 * CPU performance and should be considered when using several of these units.
 *
 * @param   in    input signal to be processed.
 * @param   freq  cutoff frequency.
 * @param   rq    the reciprocal of Q, hence bandwidth / cutoffFreq.
 */
case class BLowPass( in: UGenIn, freq: UGenIn, rq: UGenIn )
extends SingleOutUGen( in, freq, rq ) with AudioRated

object BHiPass extends UGen3RArgs {
   def ar( in: GE, freq: GE = 1200, rq: GE = 1 ) : GE =
      make( in, freq, rq )
}
/**
 * A 2nd order (12db per oct rolloff) resonant high pass filter UGen.
 * The B equalization suite is based on the Second Order Section (SOS) biquad UGen.
 *
 * Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
 * plugin UGens contain optimizations such that the coefficients get updated only when
 * there has been a change to one of the filter's parameters. This can cause spikes in
 * CPU performance and should be considered when using several of these units.
 *
 * @param   in    input signal to be processed.
 * @param   freq  cutoff frequency.
 * @param   rq    the reciprocal of Q, hence bandwidth / cutoffFreq.
 */
case class BHiPass( in: UGenIn, freq: UGenIn, rq: UGenIn )
extends SingleOutUGen( in, freq, rq ) with AudioRated

object BAllPass extends UGen3RArgs {
   def ar( in: GE, freq: GE = 1200, rq: GE = 1 ) : GE =
      make( in, freq, rq )
}
/**
 * An all pass filter UGen.
 * The B equalization suite is based on the Second Order Section (SOS) biquad UGen.
 *
 * Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
 * plugin UGens contain optimizations such that the coefficients get updated only when
 * there has been a change to one of the filter's parameters. This can cause spikes in
 * CPU performance and should be considered when using several of these units.
 *
 * @param   in    input signal to be processed.
 * @param   freq  cutoff frequency.
 * @param   rq    the reciprocal of Q, hence bandwidth / cutoffFreq.
 */
case class BAllPass( in: UGenIn, freq: UGenIn, rq: UGenIn )
extends SingleOutUGen( in, freq, rq ) with AudioRated

object BBandPass extends UGen3RArgs {
   def ar( in: GE, freq: GE = 1200, bw: GE = 1 ) : GE =
      make( in, freq, bw )
}
/**
 * An band pass filter UGen.
 * The B equalization suite is based on the Second Order Section (SOS) biquad UGen.
 *
 * Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
 * plugin UGens contain optimizations such that the coefficients get updated only when
 * there has been a change to one of the filter's parameters. This can cause spikes in
 * CPU performance and should be considered when using several of these units.
 *
 * @param   in    input signal to be processed.
 * @param   freq  center frequency.
 * @param   bw    the bandwidth '''in octaves''' between -3 dB frequencies
 */
case class BBandPass( in: UGenIn, freq: UGenIn, bw: UGenIn )
extends SingleOutUGen( in, freq, bw ) with AudioRated

object BBandStop extends UGen3RArgs {
   def ar( in: GE, freq: GE = 1200, bw: GE = 1 ) : GE =
      make( in, freq, bw )
}
/**
 * An band stop (reject) filter UGen.
 * The B equalization suite is based on the Second Order Section (SOS) biquad UGen.
 *
 * Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
 * plugin UGens contain optimizations such that the coefficients get updated only when
 * there has been a change to one of the filter's parameters. This can cause spikes in
 * CPU performance and should be considered when using several of these units.
 *
 * @param   in    input signal to be processed.
 * @param   freq  center frequency.
 * @param   bw    the bandwidth '''in octaves''' between -3 dB frequencies
 */
case class BBandStop( in: UGenIn, freq: UGenIn, bw: UGenIn )
extends SingleOutUGen( in, freq, bw ) with AudioRated

object BPeakEQ extends UGen4RArgs {
   def ar( in: GE, freq: GE = 1200, rq: GE = 1, gain: GE = 0 ) : GE =
      make( in, freq, rq, gain )
}
/**
 * An parametric equalizer UGen.
 * The B equalization suite is based on the Second Order Section (SOS) biquad UGen.
 *
 * Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
 * plugin UGens contain optimizations such that the coefficients get updated only when
 * there has been a change to one of the filter's parameters. This can cause spikes in
 * CPU performance and should be considered when using several of these units.
 *
 * @param   in    input signal to be processed.
 * @param   freq  center frequency.
 * @param   rq    the reciprocal of Q, hence bandwidth / cutoffFreq.
 * @param   gain  boost/cut at the center frequency (in decibels).
 */
case class BPeakEQ( in: UGenIn, freq: UGenIn, rq: UGenIn, gain: UGenIn )
extends SingleOutUGen( in, freq, rq, gain ) with AudioRated

object BLowShelf extends UGen4RArgs {
   def ar( in: GE, freq: GE = 1200, rs: GE = 1, gain: GE = 0 ) : GE =
      make( in, freq, rs, gain )
}
/**
 * An low shelf equalizer UGen.
 * The B equalization suite is based on the Second Order Section (SOS) biquad UGen.
 *
 * Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
 * plugin UGens contain optimizations such that the coefficients get updated only when
 * there has been a change to one of the filter's parameters. This can cause spikes in
 * CPU performance and should be considered when using several of these units.
 *
 * @param   in    input signal to be processed.
 * @param   freq  center frequency.
 * @param   rs    the reciprocal of the slope S (Shell boost/cut slope).
 *    When `S = 1`, the shelf slope is as steep as it can be and remain monotonically increasing
 *    or decreasing gain with frequency.  The shelf slope, in dB/octave, remains proportional to
 *    S for all other values for a fixed freq/sample-rate and `gain`.
 * @param   gain  boost/cut at the center frequency (in decibels).
 */
case class BLowShelf( in: UGenIn, freq: UGenIn, rs: UGenIn, gain: UGenIn )
extends SingleOutUGen( in, freq, rs, gain ) with AudioRated

object BHiShelf extends UGen4RArgs {
   def ar( in: GE, freq: GE = 1200, rs: GE = 1, gain: GE = 0 ) : GE =
      make( in, freq, rs, gain )
}
/**
 * An high shelf equalizer UGen.
 * The B equalization suite is based on the Second Order Section (SOS) biquad UGen.
 *
 * Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
 * plugin UGens contain optimizations such that the coefficients get updated only when
 * there has been a change to one of the filter's parameters. This can cause spikes in
 * CPU performance and should be considered when using several of these units.
 *
 * @param   in    input signal to be processed.
 * @param   freq  center frequency.
 * @param   rs    the reciprocal of the slope S (Shell boost/cut slope).
 *    When `S = 1`, the shelf slope is as steep as it can be and remain monotonically increasing
 *    or decreasing gain with frequency.  The shelf slope, in dB/octave, remains proportional to
 *    S for all other values for a fixed freq/sample-rate and `gain`.
 * @param   gain  boost/cut at the center frequency (in decibels).
 */
case class BHiShelf( in: UGenIn, freq: UGenIn, rs: UGenIn, gain: UGenIn )
extends SingleOutUGen( in, freq, rs, gain ) with AudioRated
