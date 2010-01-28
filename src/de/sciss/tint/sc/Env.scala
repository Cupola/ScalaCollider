/*
 *  Env.scala
 *  (ScalaCollider)
 *
 *  Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
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
 *    28-Jan-10   integrated IEnv
 */
package de.sciss.tint
package sc

import scala.collection.mutable.{ ListBuffer }
import SC._

trait EnvShape { val id: GE; val curveValue: GE = 0 }
case class varShape( override id: GE, override curveValue: GE = int2GE( 0 )) extends EnvShape
case object stepShape extends EnvShape { val id: GE = 0 }
case object linearShape extends EnvShape { val id: GE = 1 }
case object exponentialShape extends EnvShape { val id: GE = 2 }
case object sineShape extends EnvShape { val id: GE = 3 }
case object welchShape extends EnvShape { val id: GE = 4 }
case class  curveShape( override curveValue: GE ) extends EnvShape { val id: GE = 5 }
case object squaredShape extends EnvShape { val id: GE = 6 }
case object cubedShape extends EnvShape { val id: GE = 7 }

case class EnvSeg( dur: GE, targetLevel: GE, shape: EnvShape = linearShape )

import sc.{ EnvSeg => S }

trait AbstractEnvFactory[ T <: AbstractEnv ] {
   protected def create( startLevel: GE, segments: S* ) : T

	// fixed duration envelopes
	def triangle( dur: GE = 1, level: GE = 1 ) : T =  {
	  val durH = dur * 0.5f
	  create( 0, S( durH, level ), S( durH, 0 ))
	}

	def sine( dur: GE = 1, level: GE = 1 ) : T = {
	  val durH = dur * 0.5f
	  create( 0, S( durH, level, sineShape ), S( durH, 0, sineShape ))
	}

	def perc( attack: GE = 0.01f, release: GE = 1, level: GE = 1,
             shape: EnvShape = curveShape( -4 )) : T =
      create( 0, S( attack, level, shape ), S( release, 0, shape ))

	def linen( attack: GE = 0.01f, sustain: GE = 1, release: GE = 1,
              level: GE = 1, shape: EnvShape = linearShape ) : T =
		create( 0, S( attack, level, shape ), S( sustain, level, shape ),
                 S( release, 0, shape ))
}

object Env extends AbstractEnvFactory[ Env ] {
   protected def create( startLevel: GE, segments: S* ) =
      new Env( startLevel, segments )

	// envelopes with sustain
	def cutoff( release: GE = 0.1f, level: GE = 1, shape: EnvShape = linearShape ) : Env = {
      val releaseLevel: GE = shape match {
         case `exponentialShape` => 1e-05f // dbamp( -100 )
         case _ => 0
      }
		new Env( level, List( S( release, releaseLevel, shape )), 0 )
	}

	def dadsr( delay: GE = 0.1f, attack: GE = 0.01f, decay: GE = 0.3,
         	  sustainLevel: GE = 0.5f, release: GE = 1,
  				  peakLevel: GE = 1, shape: EnvShape = curveShape( -4 ),
              bias: GE = 0 ) =
      new Env( bias, List( S( delay,   bias, shape ),
                           S( attack,  peakLevel + bias, shape ),
                           S( decay,   peakLevel * sustainLevel + bias, shape ),
                           S( release, bias, shape )), 3 )

	def adsr( attack: GE = 0.01f, decay: GE = 0.3f, sustainLevel: GE = 0.5f,
             release: GE = 1, peakLevel: GE = 1, shape: EnvShape = curveShape( -4 ),
             bias: GE = 0 ) =
		new Env( bias, List( S( attack, bias, shape ),
                           S( decay, peakLevel * sustainLevel + bias, shape ),
                           S( release, bias, shape )), 2 )

	def asr( attack: GE = 0.01f, level: GE = 1, release: GE = 1,
            shape: EnvShape = curveShape( -4 )) =
		new Env( 0, List( S( attack, level, shape ), S( release, 0, shape )), 1 )
}

trait AbstractEnv {
   val startLevel: GE
   val segments: Seq[ EnvSeg ]
   def isSustained : Boolean
// note: we do not define toSeq because the format is
// significantly different so there is little sense in doing so
//	def toSeq : Seq[ GE ] ...
}

case class Env( startLevel: GE, segments: Seq[ EnvSeg ],
                releaseNode: GE = -99, loopNode: GE = -99 )
extends AbstractEnv {
  
	def toList : List[ GE ] =
      List[ GE ]( startLevel, segments.size, releaseNode, loopNode ) :::
      segments.toList.flatMap( seg =>
         List( seg.targetLevel, seg.dur, seg.shape.id, seg.shape.curveValue ))
	
//	at { arg time;
//		^this.asArray.envAt(time)
//	}
//	

   def isSustained = releaseNode != Constant( -99 )
}

object IEnv extends AbstractEnvFactory[ IEnv ] {
   protected def create( startLevel: GE, segments: S* ) =
      new IEnv( startLevel, segments )
}

case class IEnv( startLevel: GE, segments: Seq[ EnvSeg ], offset: GE = 0 )
extends AbstractEnv {
	def toList : List[ GE ] = {
      val totalDur = segments.foldLeft[ GE ]( 0 )( (sum, next) => sum + next.dur )
      List[ GE ]( offset, startLevel, segments.size, totalDur ) :::
      segments.toList.flatMap( seg =>
         List( seg.dur, seg.shape.id, seg.shape.curveValue, seg.targetLevel ))
   }

   def isSustained = false
}