/*
 *  SynthGraph.scala
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

import java.io.DataOutputStream
import ugen.{ EnvGen, Out, Silent }
import collection.breakOut
import collection.mutable.{ Buffer => MBuffer, Map => MMap, Set => MSet, Stack => MStack }
import collection.immutable.{ IndexedSeq => IIdxSeq }

/**
 *    @version 0.12, 02-Aug-10
 */
case class SynthGraph( constants: IIdxSeq[ Float ], controlValues: IIdxSeq[ Float ],
                       controlNames: IIdxSeq[ (String, Int) ], ugens: IIdxSeq[ SynthGraph.RichUGen ]) {
//   override lazy val hashCode = ... TODO: figure out how case class calculates it...
   private[synth] def write( dos: DataOutputStream ) {
      // ---- constants ----
      dos.writeShort( constants.size )
      constants.foreach( dos.writeFloat( _ ))

      // ---- controls ----
      dos.writeShort( controlValues.size )
      controlValues.foreach( dos.writeFloat( _ ))

      dos.writeShort( controlNames.size )
      var count = 0
      controlNames.foreach( name => {
         writePascalString( dos, name._1 )
         dos.writeShort( name._2 )
         count += 1
      })

//      if( verbose ) println( "ugens.size = " + ugens.size )

      dos.writeShort( ugens.size )
      ugens.foreach( ru => {
         val ugen = ru.ugen
         writePascalString( dos, ugen.name )

         dos.writeByte( ugen.rate.id )
         dos.writeShort( ugen.numInputs )
         dos.writeShort( ugen.numOutputs )
         dos.writeShort( ugen.specialIndex )

         ru.inputSpecs.foreach( spec => {
            dos.writeShort( spec._1 )
            dos.writeShort( spec._2 )
         })
         ugen.outputs.foreach( in => dos.writeByte( in.rate.id ))
      })

      dos.writeShort( 0 ) // variants not supported
   }

   @inline private def writePascalString( dos: DataOutputStream, str: String ) {
      dos.writeByte( str.size )
      dos.write( str.getBytes )
   }
}

object SynthGraph {
   def seq( elements: IIdxSeq[ UGenIn ]) : GE = {
      if( elements.size == 1 ) elements.head else new UGenInSeq( elements )
   }

   def wrapOut( thunk: => GE, fadeTime: Option[Float] = Some(0.02f) ) =
      SynthGraph {
         val res1 = thunk
         val rate = Rate.highest( res1.outputs.map( _.rate ): _* )
         if( (rate == audio) || (rate == control) ) {
            val res2 = fadeTime.map( fdt => makeFadeEnv( fdt ) * res1 ) getOrElse res1
            val out = "out".kr
            if( rate == audio ) {
               Out.ar( out, res2 )
            } else {
               Out.kr( out, res2 )
            }
         } else res1
      }

	def makeFadeEnv( fadeTime: Float ) : GE = {
		val dt			= "fadeTime".kr( fadeTime )
		val gate       = "gate".kr( 1 )
		val startVal	= (dt <= 0)
      // this is slightly more costly than what sclang does
      // (using non-linear shape plus an extra unary op),
      // but it fadeout is much smoother this way...
		EnvGen.kr( Env( startVal, List( EnvSeg( 1, 1, curveShape( -4 )), EnvSeg( 1, 0, sinShape )), 1 ),
         gate, timeScale = dt, doneAction = freeSelf ).squared
	}

   def expand( args: GE* ): Seq[ List[ UGenIn ]] = {
      var chanExp = 0
      var allOne  = true
      var hasZero = false
      for( arg <- args ) {
         chanExp = math.max( chanExp, arg.numOutputs ) // shitty implicits don't work properly
         allOne  = allOne && (arg.numOutputs == 1)
         hasZero = hasZero || (arg.numOutputs == 0)
      }
//    println( "chanExp " + chanExp + "; allOne " + allOne + "; hasZero " + hasZero )
      if( allOne ) {
         List( args.toList.flatMap( _.outputs.toList ))
      } else if( hasZero ) {
         Nil	// cannot wrap zero size seq
      } else {
         val exp = args.toList.map( _.outputs.toArray )
         val test1 = exp.toList
//         val res = for( ch <- 0 until chanExp ) yield exp.map( (arr) => arr.apply( ch % arr.size ))
         val res = for( ch <- 0 until chanExp ) yield {
            exp.map(
               (arr) => {
                  val res = arr.apply( ch % arr.size )
                  res
               }
            )
         }
         res
      }
   }

   def simplify( res: Seq[ GE ]) : GE = { // UGenIn
//    println( "simplify : " + res )
      if( res.size == 1 ) {
         res.head
      } else {
         seqOfGEToGE( res )
      }
   }

   def replaceZeroesWithSilence( ge: GE ) : GE = {
      val ins = ge.outputs
      val numZeroes = ins.foldLeft( 0 )( (sum, in) => in match {
         case Constant( 0 ) => sum + 1
         case _ => sum
      })
      if( numZeroes == 0 ) {
         ge
      } else {
         val silent = Silent.ar( numZeroes ).outputs.iterator
         val res = ins map (in => in match {
            case Constant( 0 ) => silent.next
            case _ => in
         })
         simplify( res )
      }
   }

//   private val sync        = new AnyRef
   // java.lang.ThreadLocal is around 30% faster than
   // using a synchronized map, plus we don't need
   // to look after its cleaning
   private val builders    = new ThreadLocal[ SynthGraphBuilder ] {
      override protected def initialValue = BuilderDummy
   }
   def builder: SynthGraphBuilder = builders.get

   def apply( thunk: => Unit ) : SynthGraph = {
      val b    = new BuilderImpl
      val old  = builders.get()
      builders.set( b )
      try {
         thunk
         b.build
      } finally {
         builders.set( old ) // BuilderDummy
      }
   }

   def individuate: Int = builder.individuate

  // ---- rich ugen ----

   case class RichUGen( ugen: UGen, inputSpecs: Traversable[ (Int, Int) ])

   // ---- graph builder ----

   private object BuilderDummy extends SynthGraphBuilder {
      def build : SynthGraph = error( "Out of context" )
      def addControl( values: IIdxSeq[ Float ], name: Option[ String ]) : Int = 0
      def addControlProxy( proxy: ControlProxyLike[ _ ]) {}
      def addUGen( ugen: UGen ) {}
   }

   private class BuilderImpl extends SynthGraphBuilder {
      // updated during build
      private val ugens          = MBuffer.empty[ UGen ]
      private var ugenSet        = MSet.empty[ UGen ]
      private var controlValues  = IIdxSeq.empty[ Float ]
      private var controlNames   = IIdxSeq.empty[ (String, Int) ]
      private var controlProxies = MSet.empty[ ControlProxyLike[ _ ]]

      def build = {
         val ctrlProxyMap        = buildControls
         // check inputs
         val (igens, constants)  = indexUGens( ctrlProxyMap )
         val indexedUGens        = sortUGens( igens )
         val richUGens : IIdxSeq[ RichUGen ] =
            indexedUGens.map( iu => RichUGen( iu.ugen, iu.richInputs.map( _.create )))( breakOut )
         SynthGraph( constants, controlValues, controlNames, richUGens )
      }

      private def indexUGens( ctrlProxyMap: Map[ ControlProxyLike[ _ ], (UGen, Int)]) :
         (MBuffer[ IndexedUGen ], IIdxSeq[ Float ]) = {

         val constantMap   = MMap.empty[ Float, RichConstant ]
         var constants     = IIdxSeq.empty[ Float ]
         var numIneff      = ugens.size
         val indexedUGens  = ugens.zipWithIndex.map( tup => {
            val (ugen, idx) = tup
            val eff        = ugen.isInstanceOf[ SideEffectUGen ]
            if( eff ) numIneff -= 1
            new IndexedUGen( ugen, idx, eff )
         })
         val ugenMap: Map[ UGen, IndexedUGen ] = indexedUGens.map( iu => (iu.ugen, iu))( breakOut )
         indexedUGens.foreach( iu => {
            iu.richInputs = iu.ugen.inputs.map({
               case Constant( value ) => constantMap.get( value ) getOrElse {
                  val rc         = new RichConstant( constants.size )
                  constantMap   += value -> rc
                  constants    :+= value
                  rc
               }
               case up: UGenProxy => {
                  val iui         = ugenMap( up.source )
                  iu.parents     += iui
                  iui.children   += iu
                  new RichUGenProxyBuilder( iui, up.outputIndex )
               }
               case ControlOutProxy( proxy, outputIndex, _ ) => {
                  val (ugen, off) = ctrlProxyMap( proxy )
                  val iui         = ugenMap( ugen )
                  iu.parents     += iui
                  iui.children   += iu
                  new RichUGenProxyBuilder( iui, off + outputIndex )
               }
            })( breakOut )
            if( iu.effective ) iu.richInputs.foreach( numIneff -= _.makeEffective )
         })
         val filtered = if( numIneff == 0 ) indexedUGens else {
            val res = indexedUGens.filter( _.effective )
            res foreach { iu =>
               iu.children = iu.children.filter( _.effective )
            }
            res
         }
         (filtered, constants)
      }

      /*
       *    Note that in Scala like probably in most other languages,
       *    the UGens _can only_ be added in right topological order,
       *    as that is the only way they can refer to their inputs.
       *    However, the Synth-Definition-File-Format help documents
       *    states that depth-first order is preferable performance-
       *    wise. Truth is, performance is probably the same,
       *    mNumWireBufs might be different, so it's a space not a
       *    time issue.
       */
      private def sortUGens( indexedUGens: MBuffer[ IndexedUGen ]) : Array[ IndexedUGen ] = {
         indexedUGens.foreach( iu => iu.children = iu.children.sortWith( (a, b) => a.index > b.index ))
         val sorted  = new Array[ IndexedUGen ]( indexedUGens.size )
         val avail   = MStack( indexedUGens.filter( _.parents.isEmpty ) : _* )
         var cnt     = 0
         while( avail.nonEmpty ) {
            val iu   = avail.pop
            iu.index = cnt
            sorted( cnt ) = iu
            cnt     += 1
            iu.children foreach { iuc =>
               iuc.parents.remove( iuc.parents.indexOf( iu ))
               if( iuc.parents.isEmpty ) /* avail =*/ avail.push( iuc )
            }
         }
         sorted
      }

      def addUGen( ugen: UGen ) {
         if( ugenSet.add( ugen )) ugens += ugen
      }

      def addControl( values: IIdxSeq[ Float ], name: Option[ String ]) : Int = {
         val specialIndex = controlValues.size
         controlValues ++= values
         name.foreach( n => controlNames :+= n -> specialIndex )
         specialIndex
      }

      def addControlProxy( proxy: ControlProxyLike[ _ ]) {
         controlProxies += proxy
      }

      /*
       *    Manita, how simple things can get as soon as you
       *    clean up the sclang mess...
       */
      private def buildControls: Map[ ControlProxyLike[ _ ], (UGen, Int) ] = {
         controlProxies.groupBy( _.factory ).flatMap( tuple => {
            val (factory, proxies) = tuple
            factory.build( proxies.toSeq: _* )
         })( breakOut )
      }

      // ---- IndexedUGen ----
      private class IndexedUGen( val ugen: UGen, var index: Int, var effective: Boolean ) {
         val parents    = MBuffer.empty[ IndexedUGen ]
         var children   = MBuffer.empty[ IndexedUGen ]
         var richInputs : List[ RichUGenInBuilder ] = null

         override def toString = "IndexedUGen(" + ugen + ", " + index + ", " + effective + ") : richInputs = " + richInputs
      }

      private trait RichUGenInBuilder {
         def create : (Int, Int)
         def makeEffective : Int
      }

      private class RichConstant( constIdx: Int ) extends RichUGenInBuilder {
         def create = (-1, constIdx)
         def makeEffective = 0
         override def toString = "RichConstant(" + constIdx + ")"
      }

      private class RichUGenProxyBuilder( iu: IndexedUGen, outIdx: Int ) extends RichUGenInBuilder {
         def create = (iu.index, outIdx)
         def makeEffective = {
            if( !iu.effective ) {
               iu.effective = true
               var numEff = 1
               iu.richInputs.foreach( numEff += _.makeEffective )
               numEff
            } else 0
         }
         override def toString = "RichUGenProxyBuilder(" + iu + ", " + outIdx + ")"
      }
   }
}