/*
 *  Env.scala
 *  Tintantmare
 *
 *  Copyright (c) 2008-2009 Hanns Holger Rutz. All rights reserved.
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
 */
package de.sciss.tint.sc

import _root_.de.sciss.tint.sc.Predef._

object Env {
	val shapeNames = Map( 'step -> 0, 'lin -> 1, 'linear -> 1, 'exp -> 2, 'exponential -> 2, 'sin -> 3, 'sine -> 3, 'wel -> 4, 'welch -> 4, 'sqr -> 6, 'squared -> 6, 'cub -> 7, 'cubed -> 7 )
 
//	def newClear( numSegments: Int ) : Env = {
//		^this.new(Array.fill(numSegments+1,0), Array.fill(numSegments,1))
//	}

	// fixed duration envelopes
	def triangle() : Env = triangle( 1.0f, 1.0f )
	def triangle( dur: Float ) : Env = triangle( dur, 1.0f )
	def triangle( dur: Float, level: Float ) : Env =  {
	  val durH = dur * 0.5f;
	  new Env( List( 0f, level, 0f ), List( dur, dur ), List( 1, 1 ), -99, -99 )
	}
 
	def sine() : Env = sine( 1.0f, 1.0f )
	def sine( dur: Float ) : Env = sine( dur, 1.0f )
	def sine( dur: Float, level: Float ) : Env =  {
	  val durH = dur * 0.5f;
	  new Env( List( 0f, level, 0f ), List( dur, dur ), List( 3, 3 ), -99, -99 )
	}
 
//	*perc { arg attackTime=0.01, releaseTime=1.0, level=1.0, curve = -4.0;
//		^this.new(
//			[0, level, 0],
//			[attackTime, releaseTime], 
//			curve
//		)
//	} 
//	*linen { arg attackTime=0.01, sustainTime=1.0, releaseTime=1.0, level=1.0, curve = 'lin;
//		^this.new(
//			[0, level, level, 0],
//			[attackTime, sustainTime, releaseTime], 
//			curve
//		)
//	}
//	
//	
//	// envelopes with sustain
//	*cutoff { arg releaseTime = 0.1, level = 1.0, curve = 'lin;
//		^this.new([level, 0], [releaseTime], curve, 0)
//	}
//	*dadsr { arg delayTime=0.1, attackTime=0.01, decayTime=0.3, 
//			sustainLevel=0.5, releaseTime=1.0,
//				peakLevel=1.0, curve = -4.0, bias = 0.0;
//		^this.new(
//			[0, 0, peakLevel, peakLevel * sustainLevel, 0] + bias,
//			[delayTime, attackTime, decayTime, releaseTime], 
//			curve,
//			3
//		)
//	}
//	*adsr { arg attackTime=0.01, decayTime=0.3, 
//			sustainLevel=0.5, releaseTime=1.0,
//				peakLevel=1.0, curve = -4.0, bias = 0.0;
//		^this.new(
//			[0, peakLevel, peakLevel * sustainLevel, 0] + bias,
//			[attackTime, decayTime, releaseTime], 
//			curve,
//			2
//		)
//	}
//	
//	*asr { arg attackTime=0.01, sustainLevel=1.0, releaseTime=1.0, curve = -4.0;
//		^this.new(
//			[0, sustainLevel, 0],
//			[attackTime, releaseTime], 
//			curve,
//			1
//		)
//	}
}

class Env( val levels: Seq[ GE ], val times: Seq[ GE ], val curves: Seq[ GE ],
		   val releaseNode: Int = -99, val loopNode: Int = -99 ) {
  
//  def this( levels: Seq[ GE ], times: Seq[ GE ], curves: Symbol ) =
//    this( levels, times, dup( curves, times.length ), -99, -99 )

//	var <array;

//	levels_ { arg z; 
//		levels = z;
//		array = nil;
//	} 
//	times_ { arg z; 
//		times = z;
//		array = nil;
//	} 
//	curves_ { arg z; 
//		curves = z;
//		array = nil;
//	} 
//	releaseNode_ { arg z; 
//		releaseNode = z;
//		array = nil;
//	} 
//	loopNode_ { arg z; 
//		loopNode = z;
//		array = nil;
//	}
 
//	== { arg that;
//		^this.compareObject(that,['levels','times','curves','releaseNode','loopNode'])
//	}
	
	def toArray : Array[ GE ] = {
//		if (array.isNil) { array = this.prAsArray }
//		^array
		val contents = new scala.collection.mutable.ListBuffer[ GE ]
		                                                        
        contents += levels( 0 )
        contents += times.size.toFloat
        contents += releaseNode.toFloat
        contents += loopNode.toFloat 
		for( i <- (0 until times.size) ) {
			contents += levels( i + 1 )
			contents += times( i )
			contents +=	curves( i ) // shapeNumber( curves( i ))
			contents += 0 // XXX curveValue( curves( i ))
		}
		contents.toArray
	}
	
//	at { arg time;
//		^this.asArray.envAt(time)
//	}
//	
//	asPseg {
//		var c = if(curves.isSequenceableCollection.not) { curves } { Pseq(curves) };
//		^Pseg(Pseq(levels), Pseq(times ++ [1.0]), c) // last time is a dummy
//	}
//	
	
//	releaseTime {
//		if(releaseNode.notNil,{
//			^times.copyRange(releaseNode,times.size - 1).sum
//		},{
//			^0.0 // ?
//		})
//	}

//	// blend two envelopes
//	blend { arg argAnotherEnv, argBlendFrac=0.5;
//		^this.class.new(
//			levels.blend(argAnotherEnv.levels, argBlendFrac),
//			times.blend(argAnotherEnv.times, argBlendFrac),
//			curves.blend(argAnotherEnv.curves, argBlendFrac),
//			releaseNode,
//			loopNode
//		)
//	}
//	
//	// delay the onset of the envelope
//	delay { arg delay;
//		^Env([levels[0]] ++ levels,
//			[delay] ++ times,
//			if (curves.isArray) {['lin] ++ curves} {curves},
//			if(releaseNode.notNil) {releaseNode = releaseNode + 1},
//			if(loopNode.notNil) {loopNode = loopNode + 1}
//		)
//	}
//	
//	// connect releaseNode (or end) to first node of envelope
//	circle { arg timeFromLastToFirst = 0.0, curve = 'lin';
//		var first0Then1 = Latch.kr(1.0, Impulse.kr(0.0));
//		if(releaseNode.isNil) {
//			levels = [0.0]++ levels ++ 0.0;
//			curves = [curve]++ curves.asArray.wrapExtend(times.size) ++ 'lin';
//			times  = [first0Then1 * timeFromLastToFirst] ++ times ++ inf;
//			releaseNode = levels.size - 2;
//		} {
//			levels = [0.0]++ levels;
//			curves = [curve]++ curves.asArray.wrapExtend(times.size);
//			times  = [first0Then1 * timeFromLastToFirst] ++ times;
//			releaseNode = releaseNode + 1;
//		};
//		loopNode = 0;
//	}
//	

//	isSustained {
//		^releaseNode.notNil
//	}
		
	def shapeNumber( shapeName: Symbol ) : Int = {
//		var shape;
//		if (shapeName.isValidUGenInput) { ^5 };
//		shape = shapeNames.at(shapeName);
//		if (shape.notNil) { ^shape };
//		Error("Env shape not defined.").throw;
 	   Env.shapeNames( shapeName )
	}
 
	def curveValue( curve: Symbol ) : Float = {
//		if (curve.isValidUGenInput, { ^curve },{ ^0 });
// XXX
       0
	}
	

//	test { arg releaseTime = 3.0;
//		var id, name, s;
//		s = Server.default;
//		id = s.nextNodeID;
//		name = "env_test_" ++ id;
//		SynthDef(name, { arg gate=1;
//			Out.ar(0,
//				SinOsc.ar(800, pi/2, 0.3) * EnvGen.ar(this, gate, doneAction:2)
//			)
//		}).send(s);
//		SystemClock.sched(0.2, {
//			s.sendBundle(s.latency, [9, name, id]);
//			if(this.isSustained) { s.sendBundle(s.latency + releaseTime, [15, id, 0, 0]) };
//			nil
//		});
//	}
	
//	discretize {arg n = 1024;
//		^this.asSignal(n);
//	}	
//
//	range { |lo=0, hi=1|
//		^this.class.new(levels.linlin(levels.minItem, levels.maxItem, lo, hi), 
//			times, curves, releaseNode, loopNode)
//	}
//
//	exprange { |lo=0, hi=1|
//		^this.class.new(levels.linexp(levels.minItem, levels.maxItem, lo, hi), 
//			times, curves, releaseNode, loopNode)
//	}
}