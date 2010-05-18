/*
 *  SynthDef.scala
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

import java.io.{ ByteArrayOutputStream, BufferedOutputStream, DataOutputStream, File, FileOutputStream }
import java.nio.ByteBuffer
import de.sciss.synth.{ Completion => Comp }
import ugen.Control
import osc._
import SC._
import collection.immutable.{ IndexedSeq => IIdxSeq, Iterable => IIterable, Seq => ISeq, Stack, Vector }
import collection.breakOut
import de.sciss.scalaosc.{ OSCMessage, OSCPacket }
import File.{ separator => sep }

/**
 *    @version 0.19, 17-May-10
 *    @todo    should add load and loadDir to companion object
 */
case class SynthDef private ( name: String, constants: IIdxSeq[ Float ],
                              controlValues: IIdxSeq[ Float ],
                              controlNames: IIdxSeq[ (String, Int) ],
                              ugens: IIdxSeq[ SynthDef.RichUGen ]) {

   import SynthDef._

   override def toString = "SynthDef(" + name + ")"

   def freeMsg = OSCSynthDefFreeMessage( name )

   def recv( server: Server, completion: Completion = NoCompletion ) : SynthDef = {
      if( completion.action.isDefined ) error( "Completion action not yet supported" )
      server ! recvMsg( completion.message.map( _.apply( this )))
      this
   }
  
   def recvMsg: OSCSynthDefRecvMessage = recvMsg( None )
   def recvMsg( completion: Option[ OSCMessage ]) = OSCSynthDefRecvMessage( toBytes, completion )
  
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

   def load( server: Server = Server.default, dir: String = defaultDir,
             completion: Completion = NoCompletion ) : SynthDef = {
      if( completion.action.isDefined ) error( "Completion action not yet supported" )
      writeDefFile( dir )
      server ! loadMsg( dir, completion.message.map( _.apply( this )))
      this
   }
  
   def loadMsg : OSCSynthDefLoadMessage = loadMsg()
  
   def loadMsg( dir: String = defaultDir, completion: Option[ OSCMessage ] = None ) =
	   OSCSynthDefLoadMessage( dir + sep + name + ".scsyndef", completion )

   def play: Synth = play()
   def play( target: Node = Server.default, args: Seq[ ControlSetMap ] = Nil, addAction: AddAction = addToHead ) : Synth = {
      val synth   = new Synth( target.server )
		val newMsg  = synth.newMsg( name, target, args, addAction )
		target.server ! recvMsg( newMsg )
		synth
   }
    
   def writeDefFile : Unit = writeDefFile()

   def writeDefFile( dir: String = defaultDir, overwrite: Boolean = false ) {
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
   def addControlProxy( proxy: ControlProxyLike[ _ ]) : Unit
   def addControl( values: IIdxSeq[ Float ], name: Option[ String ]) : Int
   def build( name: String ) : SynthDef

   private var indivCnt = 0
   def individuate : Int = {
      val res = indivCnt
      indivCnt += 1
      res
   }
}

object SynthDef {
   type Completion   = Comp[ SynthDef ]
   val NoCompletion  = Comp[ SynthDef ]( None, None )  

   var defaultDir    = System.getProperty( "java.io.tmpdir" )

//   private val sync        = new AnyRef
   // java.lang.ThreadLocal is around 30% faster than
   // using a synchronized map, plus we don't need
   // to look after its cleaning
   private val builders    = new ThreadLocal[ SynthDefBuilder ] {
      override protected def initialValue = BuilderDummy
   }
   def builder: SynthDefBuilder = builders.get

   def apply( name: String )( thunk: => GE ) : SynthDef = {
      val b = new BuilderImpl
      builders.set( b )
      try {
         thunk
         b.build( name )
      } finally {
         builders.set( BuilderDummy )
      }
   }

   def recv( server: Server = Server.default, name: String, completion: Completion = NoCompletion )
           ( thunk: => GE ) : SynthDef = {
      val d = apply( name )( thunk )
      d.recv( server, completion )
      d
   }

   def individuate: Int = builder.individuate

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

  // ---- rich ugen ----

   case class RichUGen( ugen: UGen, inputSpecs: Traversable[ (Int, Int) ])

   // ---- graph builder ----

   private object BuilderDummy extends SynthDefBuilder {
      def build( name: String ) : SynthDef = error( "Out of context" )
      def addControl( values: IIdxSeq[ Float ], name: Option[ String ]) : Int = 0
      def addControlProxy( proxy: ControlProxyLike[ _ ]) {}
      def addUGen( ugen: UGen ) {}
   }

   private class BuilderImpl extends SynthDefBuilder {
      // updated during build
      private var ugens : IIdxSeq[ UGen ]                   = Vector.empty
      private var ugenSet                                   = Set.empty[ UGen ]
      private var controlValues: IIdxSeq[ Float ]           = Vector.empty
      private var controlNames: IIdxSeq[ (String, Int) ]    = Vector.empty
      private var controlProxies                            = Set.empty[ ControlProxyLike[ _ ]]

      def build( name: String ) = {
         val ctrlProxyMap        = buildControls
         // check inputs
         val (igens, constants)  = indexUGens( ctrlProxyMap )
         val indexedUGens        = sortUGens( igens )
         val richUGens           = indexedUGens.map( iu => RichUGen( iu.ugen, iu.richInputs.map( _.create )))
         new SynthDef( name, constants, controlValues, controlNames, richUGens )
      }

      private def indexUGens( ctrlProxyMap: Map[ ControlProxyLike[ _ ], (UGen, Int)]) :
         (IIdxSeq[ IndexedUGen ], IIdxSeq[ Float ]) = {

         var constantMap   = Map.empty[ Float, RichConstant ]
         var constants     = Vector.empty[ Float ]
         val indexedUGens  = ugens.zipWithIndex.map( tup => new IndexedUGen( tup._1, tup._2 ))
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
                  iu.parents    :+= iui
                  iui.children  :+= iu
                  new RichUGenProxyBuilder( iui, up.outputIndex )
               }
               case ControlOutProxy( proxy, outputIndex, _ ) => {
                  val (ugen, off) = ctrlProxyMap( proxy )
                  val iui         = ugenMap( ugen )
                  iu.parents    :+= iui
                  iui.children  :+= iu
                  new RichUGenProxyBuilder( iui, off + outputIndex )
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
      private def sortUGens( indexedUGens: IIdxSeq[ IndexedUGen ]) : IIdxSeq[ IndexedUGen ] = {
         indexedUGens.foreach( iu => iu.children = iu.children.sortWith( (a, b) => a.index > b.index ))
         var sorted  = Vector.empty[ IndexedUGen ]
         var avail   = Stack( indexedUGens.filter( _.parents.isEmpty ) : _* )
         while( avail.nonEmpty ) {
            val iu   = avail.top
            avail    = avail.pop
            iu.index = sorted.size
            sorted :+= iu
            iu.children.foreach( iuc => {
               iuc.parents = iuc.parents.patch( iuc.parents.indexOf( iu ), Nil, 1 ) // why so difficult to remove?
               if( iuc.parents.isEmpty ) avail = avail.push( iuc )
            })
         }
         sorted
      }

      def addUGen( ugen: UGen ) {
//         if( verbose ) println( "ADD UNIT " + ugen.name + " -> index " + ugensUnsorted.size )
//         ugens ::= ugen
         if( !ugenSet.contains( ugen )) {
            ugenSet += ugen
            ugens  :+= ugen
         }
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
      private def buildControls: Map[ ControlProxyLike[ _ ], (UGen, Int) ] =
         controlProxies.groupBy( _.factory ).flatMap( tuple => {
            val (factory, proxies) = tuple
            factory.build( proxies.toSeq: _* )
         })( breakOut )

      // ---- IndexedUGen ----
      private class IndexedUGen( val ugen: UGen, var index: Int ) {
         var parents : IIdxSeq[ IndexedUGen ]   = Vector.empty
         var children  : IIdxSeq[ IndexedUGen ] = Vector.empty
         var richInputs : List[ RichUGenInBuilder ] = null
      }

      private trait RichUGenInBuilder {
         def create : (Int, Int)
      }

      private class RichConstant( constIdx: Int ) extends RichUGenInBuilder {
         def create = (-1, constIdx)
      }

      private class RichUGenProxyBuilder( iu: IndexedUGen, outIdx: Int ) extends RichUGenInBuilder {
         def create = (iu.index, outIdx)
      }
   }
}