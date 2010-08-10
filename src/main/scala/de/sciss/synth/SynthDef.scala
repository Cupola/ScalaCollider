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
import osc.{ OSCSynthDefFreeMessage, OSCSynthDefLoadMessage, OSCSynthDefRecvMessage }
import collection.immutable.{ IndexedSeq => IIdxSeq, Iterable => IIterable, Seq => ISeq, Stack, Vector }
import collection.breakOut
import de.sciss.osc.{ OSCMessage, OSCPacket }
import File.{ separator => sep }

/**
 *    @version 0.19, 17-May-10
 *    @todo    should add load and loadDir to companion object
 */
case class SynthDef( name: String, graph: SynthGraph ) {

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
      graph.write( dos )
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

   def playX: Synth = play()
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
      graph.ugens.foreach( ru => {
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
      graph.ugens.foreach( ru => {
         println( "#" + i + " : " + ru.ugen.name +
            (if( ru.ugen.specialIndex != 0 ) "-" + ru.ugen.specialIndex else "") + ru.inputSpecs.map({
               case (-1, idx)    => graph.constants( idx ).toString
               case (uidx, oidx) => { val ru = graph.ugens( uidx ); "#" + uidx + " : " + ru.ugen.name +
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

object SynthDef {
   type Completion   = Comp[ SynthDef ]
   val NoCompletion  = Comp[ SynthDef ]( None, None )

   var defaultDir    = System.getProperty( "java.io.tmpdir" )

   def apply( name: String )( thunk: => Unit ) : SynthDef = SynthDef( name, SynthGraph( thunk ))

   def recv( server: Server = Server.default, name: String, completion: Completion = NoCompletion )
           ( thunk: => Unit ) : SynthDef = {
      val d = apply( name )( thunk )
      d.recv( server, completion )
      d
   }

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
}