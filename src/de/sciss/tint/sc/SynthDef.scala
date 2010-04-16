/*
 *  SynthDef.scala
 *  Tintantmare
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
 */

package de.sciss.tint.sc

import java.io.{ ByteArrayOutputStream, BufferedOutputStream, DataOutputStream,
                        File, FileOutputStream }
import java.nio.ByteBuffer
import de.sciss.tint.sc.ugen.{ Control }
import SC._
import collection.immutable.{ IndexedSeq => IIdxSeq, Iterable => IIterable, Seq => ISeq, Stack, Vector }
import collection.{ breakOut }
import de.sciss.scalaosc.{ OSCMessage, OSCPacket }

/**
 *    @version 0.17, 14-Apr-10
 */
class SynthDef private ( val name: String, constants: IIdxSeq[ Float ], controlValues: IIdxSeq[ Float ],
                         controlDescs: IIdxSeq[ ControlDesc ], ugens: IIdxSeq[ SynthDef.RichUGen ]) {

   import SynthDef._

   def freeMsg: OSCMessage = OSCMessage( "/d_free", name )

   def send( server: Server ) : SynthDef = {
      server.sendMsg( recvMsg )
      this
   }
  
   def send( server: Server, completionMsg: OSCMessage ) : SynthDef = {
      server.sendMsg( recvMsg( completionMsg ))
      this
   }
  
   def recvMsg = OSCMessage( "/d_recv", toBytes )

   def recvMsg( completionMsg: OSCMessage ) =
	   OSCMessage( "/d_recv", toBytes, completionMsg )
  
  	def toBytes : ByteBuffer = {
    	val baos	= new ByteArrayOutputStream
    	val dos	= new DataOutputStream( baos )

    	dos.writeInt( 0x53436766 )	// magic cookie 'SCgf'
    	dos.writeInt( 1 )			   // version
    	dos.writeShort( 1 ) 		   // number of defs in file.
    	write( dos )
    	dos.flush
    	dos.close

    	ByteBuffer.wrap( baos.toByteArray ).asReadOnlyBuffer()
   }

   private def write( dos: DataOutputStream ) {
      writePascalString( dos, name )

      // ---- constants ----
      dos.writeShort( constants.size )
      constants.foreach( c => dos.writeFloat( c.value ))

      // ---- controls ----
      dos.writeShort( controlValues.size )
      controlValues.foreach( dos.writeFloat( _ ))

      dos.writeShort( controlDescs.size )
      var count = 0
      controlDescs.foreach( desc => {
         desc.name.map( name => {
            writePascalString( dos, name )
            dos.writeShort( desc.ugen.specialIndex + desc.ugenOutputIndex )
         }) getOrElse {
            println( "Warning: unnamed control " + count + " dropped." )
         }
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
         ugen.outputRates.foreach( rate => dos.writeByte( rate.id ))
      })

      dos.writeShort( 0 ) // variants not supported
   }

   def load( server: Server ) : SynthDef = {
      load( server, SynthDef.synthDefDir )
   }
  
   def load( server: Server, completionMsg: OSCMessage ) : SynthDef = {
      load( server, completionMsg, SynthDef.synthDefDir )
   }
  
   def load( server: Server, completionMsg: OSCMessage, dir: String ) : SynthDef = {
      writeDefFile( dir )
      server.sendMsg( loadMsg( completionMsg, dir ))
      this
   }
  
   def load( server: Server, dir: String ) : SynthDef = {
      writeDefFile( dir )
      server.sendMsg( loadMsg( dir ))
      this
   }
  
   def loadMsg( completionMsg: OSCMessage ) : OSCMessage = {
      loadMsg( completionMsg, SynthDef.synthDefDir )
   }
  
   def loadMsg( completionMsg: OSCMessage, dir: String ) =
	   OSCMessage( "/d_load", dir + name + ".scsyndef", completionMsg )
  
   def loadMsg : OSCMessage = loadMsg( SynthDef.synthDefDir )
  
   def loadMsg( dir: String ) =
	   OSCMessage( "/d_load", dir + name + ".scsyndef" )

   def play( target: Node = Server.default, args: Seq[ Tuple2[ String, Float ]] = Nil, addAction: AddAction = addToHead ) : Synth = {
      val synth	= new Synth( name, target.server )
		val msg		= synth.newMsg( target, args, addAction )
		send( target.server, msg )
		synth
   }
    
   def writeDefFile {
      writeDefFile( SynthDef.synthDefDir, false )
   }

   def writeDefFile( dir: String ) {
      writeDefFile( dir, false )
   }

   def writeDefFile( overwrite: Boolean ) {
      writeDefFile( SynthDef.synthDefDir, overwrite )
   }

   def writeDefFile( dir: String, overwrite: Boolean ) {
      var file = new File( dir, name + ".scsyndef" )
      val exists = file.exists
      if( overwrite ) {
         if( exists ) file.delete
         SynthDef.writeDefFile( file.getAbsolutePath, List( this ))
      } else if( !exists ) {
         SynthDef.writeDefFile( file.getAbsolutePath, List( this ))
      }
   }
  
   @inline private def writePascalString( dos: DataOutputStream, str: String ) {
      dos.writeByte( str.size )
      dos.write( str.getBytes )
   }

   def hexDump {
      OSCPacket.printHexOn( Console.out, toBytes )
   }

   def testTopoSort {
      var i = 0
      ugens.foreach( ru => {
         var j = 0
         ru.inputSpecs.foreach( spec => {
            if( (spec._1 >= 0) && (spec._1 <= i) ) {
               error( "Test failed : ugen " + i + " = " + ru.ugen + " -> input " + j + " = " + spec )
            }
            j += 1
         })
         i += 1
      })
      println( "Test succeeded." )
   }

   def debugDump {
      var i = 0
      ugens.foreach( ru => {
         println( "#" + i + " : " + ru.ugen.name +
            (if( ru.ugen.specialIndex != 0 ) "-" + ru.ugen.specialIndex else "") + ru.inputSpecs.map({
               case (-1, idx)    => constants( idx ).toString
               case (uidx, oidx) => { val ru = ugens( uidx ); "#" + uidx + " : " + ru.ugen.name +
                  (if( oidx > 0 ) "@" + oidx else "") }
            }).mkString( "( ", ", ", " )" ))
         i += 1
      })
   }

//   // test method
//   def testTopologicalOrder {
//      var known   = Set[ UGen ]()
//      val success = ugens.forall( ugen => {
//         val u    = ugen.inputs.partialMap { case up: UGenProxy => up.source }
//         val res  = u.forall( known.contains( _ ))
//         known   += ugen
//         res
//      })
//      if( !success ) error( "Test failed" )
//      println( "Test passed" )
//   }

   
/*
  private def writeInputSpec( dos: DataOutputStream, inp: UGenInput ) {
    if( inp.isInstanceOf[ OutputProxy ]) {
      val proxy			= inp.asInstanceOf[ OutputProxy ]
      val synthIndex	= if( proxy.source.isInstanceOf[ UGen ]) ugens.indexOf( proxy.source ) else
      if( synthIndex == -1 ) throw new IOException( "UGen not listed in graph function : " + inp )
      dos.writeShort( synthIndex )
      dos.writeShort( proxy.channel )
      
      println( "wrote proxy, src = " + proxy.source + "; channel " + proxy.channel + "; synthIndex " + synthIndex )
      
    } else if( inp.isInstanceOf[ Constant ]) {
      val constIndex = constants.indexOf( inp );
      if( constIndex == -1 ) throw new IOException( "Constant not listed in synth def : " + inp )
      dos.writeShort( -1 )
      dos.writeShort( constIndex )

      println( "wrote constant, value = " + inp.asInstanceOf[ Constant ].value + "; constIndex " + constIndex )

    } else {
      assume( false )
    }
  }
*/

//   private def checkInputs {
//      var seenErr = false
//      ugens.foreach( ugen => {
//         val err = ugen.checkInputs
//         if( err.isDefined ) {
//            seenErr = true
//            if( verbose ) println( ugen.getClass.toString + " " + err )
////          ugen.dumpArgs
//         }
//      })
//      if( seenErr ) { throw new Exception( "SynthDef " + name + " build failed" )}
//   }
}

trait SynthDefBuilder {
   def addUGen( ugen: UGen ) : Unit
   def addControlDesc( desc: ControlDesc ) : Unit
   def addControl( u: Control ) : Int
   def build( name: String ) : SynthDef
   def individuate : Int
}

object SynthDef {
//	var verbose = false
//   var buildGraph: Option[ UGenGraph ] = None
   var synthDefDir         = System.getProperty( "java.io.tmpdir" )

   private val sync        = new AnyRef
   private var builders    = Map.empty[ Thread, SynthDefBuilder ]
   def builder: Option[ SynthDefBuilder ] = builders.get( Thread.currentThread )

//  def apply( name: String, func: => Any ) = new SynthDef( name, func, Nil, Nil, Nil )
   def apply( name: String )( thunk: => GE ) : SynthDef = {
      val b = buildUGenGraph( thunk )
      b.build( name )
   }

   def individuate: Int = builder.map( _.individuate ) getOrElse 0

//   def wrap( ugenGraphFunc: () => GE ) : GE = {
//	   if( buildSynthDef.isEmpty ) {
//	      throw new Exception( "SynthDef.wrap should be called inside a SynthDef ugenGraphFunc." )
//	   }
//      buildSynthDef.get.buildUGenGraph( ugenGraphFunc )
//   }

   def writeDefFile( path: String, defs: Seq[ SynthDef ]) {
      val os	= new FileOutputStream( path )
	   val dos	= new DataOutputStream( new BufferedOutputStream( os ))
 
//    try {
      dos.writeInt( 0x53436766 ) 		// magic cookie
      dos.writeInt( 1 ) 				   // version
      dos.writeShort( defs.size ) 		// number of defs in file.
      defs.foreach( _.write( dos ))
//    }
//    finally {
      dos.close
//    }
   }

   private def buildUGenGraph( thunk: => Any ) : SynthDefBuilder = {
      val b = new BuilderImpl
      val t = Thread.currentThread
      sync.synchronized {
         builders += t -> b
      }
      try {
         thunk
         b.finish
         b
      } finally {
         sync.synchronized {
            builders -= t
         }
      }
   }

   // ---- rich ugen ----

   case class RichUGen( ugen: UGen, inputSpecs: Traversable[ Tuple2[ Int, Int ]])

   // ---- graph builder ----

   private class BuilderImpl extends SynthDefBuilder {
      // updated during build
      private var ugens                                  = Set.empty[ UGen ]
      private var controlValues: IIdxSeq[ Float ]        = Vector.empty
      private var controlDescs: IIdxSeq[ ControlDesc ]   = Vector.empty
//      private var controlDescMap                         = Set.empty[ String, ControlDesc ]

      // build results
      private var finished                               = false
      private var constants : IIdxSeq[ Float ]           = null
      private var richUGens : IIdxSeq[ RichUGen ]        = null

      def finish {
         require( !finished )
         finished = true

         buildControls
         // check inputs
         val (igens, c)    = indexUGens
         val indexedUGens  = sortUGens( igens )
         richUGens         = indexedUGens.map( iu => RichUGen( iu.ugen, iu.richInputs.map( _.create ))) 
         constants         = c
      }

      def build( name: String ) = {
         new SynthDef( name, constants, controlValues, controlDescs, richUGens )
      }

      private var indivCnt = 0
      def individuate = {
         val res = indivCnt
         indivCnt += 1
         res
      }

      private def indexUGens : Tuple2[ IIterable[ IndexedUGen ], IIdxSeq[ Float ]] = {
         var constantMap   = Map.empty[ Float, RichConstant ]
         var constants     = Vector.empty[ Float ]
         val indexedUGens  = ugens.map( new IndexedUGen( _ ))
//         val ugenMap       = indexedUGens.map( iu => (iu.ugen, iu)).toMap
         val ugenMap: Map[ UGen, IndexedUGen ] = indexedUGens.map( iu => (iu.ugen, iu))( breakOut )
         indexedUGens.foreach( iu => {
            iu.richInputs = iu.ugen.inputs.collect({
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
            })( breakOut )
         })
         (indexedUGens, constants)
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
      private def sortUGens( indexedUGens: IIterable[ IndexedUGen ]) : IIdxSeq[ IndexedUGen ] = {
         var sorted  = Vector.empty[ IndexedUGen ]
         val start   = indexedUGens.filter( _.parents.isEmpty )
         val sorting = (a: IndexedUGen, b: IndexedUGen) => a.numWireBufs > b.numWireBufs
         var stack   = Stack( start.toList.sortWith( sorting ).iterator )
         while( stack.nonEmpty ) {
            var iter = stack.top
            stack    = stack.pop
            while( iter.hasNext ) {
               val iu   = iter.next
               iu.index = sorted.size
               sorted :+= iu
               val c    = iu.children
               c.foreach( _.parents -= iu )
               val availc  = c.toList.filter( _.parents.isEmpty ).sortWith( sorting )
               if( availc.nonEmpty ) {
                  stack +:= iter
                  iter    = availc.iterator
               }
            }
         }
         sorted
      }

      def addUGen( ugen: UGen ) {
//         if( verbose ) println( "ADD UNIT " + ugen.name + " -> index " + ugensUnsorted.size )
//         ugens ::= ugen
         ugens += ugen
      }

      def addControl( u: Control ) : Int = {
         val specialIndex = controlValues.size
         controlValues ++= u.values
         specialIndex
      }

      def addControlDesc( desc: ControlDesc ) {
//         if( verbose ) println( "ADD CONTROL DESC " + desc.name.getOrElse( "<noname>" ))
         controlDescs :+= desc
//         desc.name.foreach( controlDescMap += _ -> desc )
      }

      private def buildControls {
//         if( verbose ) println( "buildControls" )

          val irControlDescs	= controlDescs.filter( _.rate == scalar )
          val krControlDescs	= controlDescs.filter( _.rate == control )
// XXX tr currently broken
//    val trControlDescs	= controlDescs.filter( _.rate == trigger )

//    if (nonControlNames.size > 0) {
//      nonControlNames.do {|cn|
//                          arguments[cn.argNum] = cn.defaultValue;
//      };
//    };

         if( irControlDescs.size > 0 ) {
//            if( verbose ) println( "irControlDescs.size = " + irControlDescs.size )
            val ctrl = Control.ir( irControlDescs.flatMap( _.initValues ))
            setControlDescSource( irControlDescs, ctrl )
         }
// XXX tr currently broken
//	if( trControlDescs.size > 0 ) {
//	  if( verbose ) println( "trControlDescs.size = " + trControlDescs.size )
//      val ctrl = TrigControl.kr( trControlDescs.flatMap( _.initValues ))
//      setControlDescSource( trControlDescs, ctrl )
//	}
         if( krControlDescs.size > 0 ) {
            val krControlDescsPlain 	= krControlDescs.filter( _.lag.isEmpty )
            val krControlDescsLagged	= krControlDescs.filter( _.lag.isDefined )

            if( krControlDescsPlain.size > 0 ) {
//               if( verbose ) println( "krControlDescsPlain.size = " + krControlDescsPlain.size )
               val ctrl = Control.kr( krControlDescsPlain.flatMap( _.initValues ))
               setControlDescSource( krControlDescsPlain, ctrl )
            }
            if( krControlDescsLagged.size > 0 ) {
//               if( verbose ) println( "XXX krControlDescsLagged NOT YET IMPLEMENTED" )
            }
         }
      }

//      def collectConstants {
//         ugensUnsorted.foreach( iu => {
//            constantSet ++= iu.ugen.inputs.collect { case c: Constant => c }
//         })
//      }

      private def setControlDescSource( descs: Seq[ ControlDesc ], source: UGen ) {
         var off	= 0
         descs.foreach( desc => {
            desc.ugen				= source
            desc.ugenOutputIndex	= off
            off += desc.numOutputs
         })
      }

      // ---- IndexedUGen ----
      private class IndexedUGen( val ugen: UGen ) {
         var parents       = Set.empty[ IndexedUGen ]
         var children      = Set.empty[ IndexedUGen ]
         val numWireBufs   = ugen.outputRates.count( _ == audio )
         var index         = -2
         var richInputs : List[ RichUGenInBuilder ] = null
      }

      private trait RichUGenInBuilder {
         def create : Tuple2[ Int, Int ]
      }

      private class RichConstant( constIdx: Int ) extends RichUGenInBuilder {
         def create = (-1, constIdx)
      }

      private class RichUGenProxyBuilder( iu: IndexedUGen, outIdx: Int ) extends RichUGenInBuilder {
         def create = (iu.index, outIdx)
      }
   }
}