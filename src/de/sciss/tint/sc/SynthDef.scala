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

import de.sciss.scalaosc.OSCMessage
import scala.collection.mutable.{ HashMap, HashSet, ListBuffer, Map, Set }
import java.io.{ ByteArrayOutputStream, BufferedOutputStream, DataOutputStream,
                        File, FileOutputStream }
import java.nio.ByteBuffer

import de.sciss.tint.sc.ugen.{ Control }
import SC._
//import _root_.scala.Predef._
//import Rates._

/**
 * 	@author		Hanns Holger Rutz
 *	@version	0.16, 18-Jan-10
 */
class SynthDef( val name: String, rates: Seq[Any] = Nil, prependArgs: Seq[Any] = Nil, var variants: Seq[Any] = Nil )( ugenGraphFunc: () => GE ) {
//  private def ugenGraphFunc() = func
//  private var controlIndex								= 0
	private val controlValues							= new ListBuffer[ Float ]()
  private val constantSet : Set[ Constant ]				= new HashSet[ Constant ]()
  private val constants : ListBuffer[ Constant ]		= new ListBuffer[ Constant ]()
  private val controlDescMap : Map[ String, ControlDesc ] = new HashMap[ String, ControlDesc ]()
  private val controlDescs : ListBuffer[ ControlDesc ]	= new ListBuffer[ ControlDesc ]()
  private var ugens	: ListBuffer[ UGen ]				= new ListBuffer[ UGen ]()
  
  // XXX BEGIN XXX
  var available : ListBuffer[ UGen ]					= new ListBuffer[ UGen ]()
  // XXX END XXX

  import SynthDef._

  build
  
/*
  def this( name: String, ugenGraphFunc: () => Any, rates: Seq[Any], prependArgs: Seq[Any]) {
    this( name, ugenGraphFunc, rates, prependArgs, Nil )
  }
  
  def this( name: String, ugenGraphFunc: () => Any, rates: Seq[Any]) {
    this( name, ugenGraphFunc, rates, Nil, Nil )
  }

  def this( name: String, ugenGraphFunc: () => Any) {
    this( name, ugenGraphFunc, Nil, Nil, Nil )
  }
*/
  
  def getControlDesc( name: String ) = controlDescMap( name )
  def getConstantIndex( c: Constant ) = constants.indexOf( c ) // XXX not efficient, should be a map?
//  def getUGenIndex( u: UGen ) = ugens.indexOf( u )
  
//  def allocControl( numCh: Int ) : Int = { val result = controlIndex; controlIndex += numCh; result }

	def addControl( u: Control ) : Int = {
		val specialIndex = controlValues.size
//		u.specialIndex = controlValues.size
		controlValues ++= u.values
		specialIndex
	}
  
//  def addControl( u: Control ) : Int = {
//    val result = controlIndex;
//    controlIndex += numCh; result
//  }
  
  def send( server: Server ) : SynthDef = {
    server.sendMsg( recvMsg )
    this
  }
  
  def send( server: Server, completionMsg: OSCMessage ) : SynthDef = {
    // XXX completionMsg.asRawOSC !!
    server.sendMsg( recvMsg( completionMsg ))
    //server.sendMsg("/d_recv", this.asBytes,completionMsg);
    this
  }
  
  def recvMsg = OSCMessage( "/d_recv", toBytes )

  def recvMsg( completionMsg: OSCMessage ) =
	  OSCMessage( "/d_recv", toBytes, completionMsg )
  
  protected def build {
    initBuild
    buildUGenGraph( ugenGraphFunc, rates, prependArgs )
    finishBuild
  }

  	def toBytes : ByteBuffer = {
    	val baos	= new ByteArrayOutputStream
    	val dos		= new DataOutputStream( baos )

    	dos.writeInt( 0x53436766 )	// magic cookie 'SCgf'
    	dos.writeInt( 1 )			// version
    	dos.writeShort( 1 ) 		// number of defs in file.
    	write( dos )
    	dos.flush
    	dos.close

    	ByteBuffer.wrap( baos.toByteArray ).asReadOnlyBuffer()
//    	val arr = baos.toByteArray
//    	val bb = ByteBuffer.allocate( arr.length )
//    	bb.put( arr )
//    	bb.flip
//    	bb
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
//		target = target.asTarget
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
  
  private def writePascalString( dos: DataOutputStream, str: String ) {
    dos.writeByte( str.size );
    dos.write( str.getBytes );
  }

  private def write( dos: DataOutputStream ) {

    writePascalString( dos, name )	
    writeConstants( dos )

//    dos.writeShort( controlDescs.size )
//    controlDescs.foreach (desc => dos.writeFloat( desc.initValues.head )) // XXX .first
	dos.writeShort( controlValues.size )
	controlValues.foreach( dos.writeFloat( _ ))
		
    dos.writeShort( controlDescs.size )
    var count = 0
    controlDescs.foreach (desc => {
//		println( "||||||| count = " + count + "; name " + desc.name.getOrElse( "<unnamed>" ) + "; specialIndex " + (desc.ugen.specialIndex + desc.ugenOutputIndex))
      if( desc.name.isDefined ) {
        writePascalString( dos, desc.name.get )
//		dos.writeShort( desc.getIndex() )
//        dos.writeShort( count )
          dos.writeShort( desc.ugen.specialIndex + desc.ugenOutputIndex )
      } else {
        println( "Warning: unnamed control " + count + " dropped." )
      }
      count += 1
    })
	
    if( verbose ) println( "ugens.size = " + ugens.size )
    
    dos.writeShort( ugens.size )
    ugens.foreach (ugen => writeUGenSpec( dos, ugen ))
		
    dos.writeShort( variants.size )
    if( variants.size > 0 ) {
      throw new IllegalStateException( "Variants : not supported!!" )
    }
  }

  private def writeConstants( dos: DataOutputStream ) {
    dos.writeShort( constants.size );
    constants.foreach (con => dos.writeFloat( con.value ))
  }

  private def writeUGenSpec( dos: DataOutputStream, ugen: UGen ) {
    writePascalString( dos, ugen.name )

	if( verbose ) println( "writing ugen spec, name = " + ugen.name + "; index " + ugen.synthIndex + "; numInputs = " + ugen.numInputs + "; numOutputs " + ugen.numOutputs + "; specialIndex " + ugen.specialIndex )

//  dos.writeByte( UGen.getRateID( ugen.rate ))
    dos.writeByte( ugen.rate.id )
    dos.writeShort( ugen.numInputs )
    dos.writeShort( ugen.numOutputs )
    dos.writeShort( ugen.specialIndex )
    
//  ugen.inputs.foreach (input => writeInputSpec( dos, input ))
    ugen.inputs.foreach (ugen => {
      if( verbose ) println( "... input:" )
      ugen.writeInputSpec( dos, this )
    })
//    ugen.outputRates.foreach (rate => dos.writeByte( UGen.getRateID( rate )))
    for( rate <- ugen.outputRates ) dos.writeByte( rate.id )
  }

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

  private def initBuild {
    SynthDef.buildSynthDef	= Some( this )
	constants.clear
    constantSet.clear
    controlDescs.clear
//    controlIndex			= 0;
		controlValues.clear
  }
  
  private def finishBuild {
    optimizeGraph
    collectConstants
    checkInputs		// will die on error
		
    // re-sort graph. reindex.
    topologicalSort
    indexUGens
    SynthDef.buildSynthDef = None		
  }
  
  private def optimizeGraph {
    initTopoSort
    ugens.clone.foreach (_.optimizeGraph)
  }

  private def collectConstants {
    ugens.foreach (ugen => {
      ugen.inputs.foreach (input => {
        if( input.isInstanceOf[ Constant ]) addConstant( input.asInstanceOf[ Constant ])
      })
    })
  }

  def addConstant( value: Constant ) = {
    if( !constantSet.contains( value )) {
      constantSet += value
      constants	  += value
    }
  }

  def addUGen( ugen: UGen ) {
    if( verbose ) println( "ADD UNIT " + ugen.name + " -> index " + ugens.size );
    ugen.synthIndex = ugens.size;
    ugens += ugen
  }
  
  def addControlDesc( desc: ControlDesc ) {
    if( verbose ) println( "ADD CONTROL DESC " + desc.name.getOrElse( "<noname>" ));
    controlDescs += desc
	desc.name.foreach( controlDescMap( _ ) = desc )
  }

  private def indexUGens {
    var count = 0
    ugens.foreach( ugen => {
    	ugen.synthIndex = count
    	count += 1
    })
    if( verbose ) println( "INDEXED " + count )
  }
  
  private def initTopoSort {
    available.clear
    if( verbose ) println( "before sort: " + ugens.size )
    ugens.foreach (ugen => {
      ugen.antecedents.clear
      ugen.descendants.clear
    })
    ugens.foreach (_.initTopoSort)	// this populates the descendants and antecedents
    ugens.reverse.foreach (ugen => {
      val sorted = ugen.descendants.toList.sortWith( (a, b) => a.synthIndex < b.synthIndex )
      ugen.descendants.clear
      ugen.descendants.appendAll( sorted )
//    ugen.makeAvailable; // all ugens with no antecedents are made available
      if( ugen.antecedents.isEmpty ) {
        available.append( ugen );
      }

    })
    if( verbose ) println( "after sort: " + ugens.size )
  }
  
  private def cleanupTopoSort {
    ugens.foreach (ugen => {
      ugen.antecedents.clear
      ugen.descendants.clear
    })
  }
 
  private def topologicalSort {
    val outStack =  new ListBuffer[ UGen ]()
    initTopoSort
    while( available.size > 0 ) {
      val ugen = available.remove( available.size - 1 )
//    ugen.schedule( outStack )
      ugen.descendants.reverse.foreach (descUGen => {
//      descUGen.removeAntecedent( ugen )
        descUGen.antecedents -= ugen
//      descUGen.makeAvailable
        if( descUGen.antecedents.isEmpty ) {
	      available.append( descUGen );
        }
      })
      outStack.append( ugen )
    }
    ugens.clear
    ugens.appendAll( outStack )
    cleanupTopoSort
  }

  private def checkInputs {
    var seenErr = false;
    ugens.foreach (ugen => {
      val err = ugen.checkInputs
      if( err.isDefined ) { 
        seenErr = true;
        if( verbose ) println( ugen.getClass.toString + " " + err )
        ugen.dumpArgs
      }
    })
  if( seenErr ) { throw new Exception( "SynthDef " + name + " build failed" )}
    }
    
  protected def buildUGenGraph( func: () => GE, rates: Seq[Any], prependArgs: Seq[Any] ) : GE = {

    // save/restore controls in case of *wrap
//    var saveControls = controlDescs; // XXX
		
    controlDescs.clear
		
//  	addControlsFromArgsOfFunc( func, rates, prependArgs.size );
// XXX    val result = func.valueArray( prependArgs ++ this.buildControls );
	val result = func.apply() // ( Nothing )
	buildControls	// ! needs to be _after_ evaluating func
   
// XXX    controls = saveControls
//val result = new GESeq( Nil ) // XXX
    result
  }

  private def buildControls {
    if( verbose ) println( "buildControls" )
    
//  var nonControlDescs	= controlDescs.filter( _.rate == none )
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
	  if( verbose ) println( "irControlDescs.size = " + irControlDescs.size )
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
        if( verbose ) println( "krControlDescsPlain.size = " + krControlDescsPlain.size )
        val ctrl = Control.kr( krControlDescsPlain.flatMap( _.initValues ))
        setControlDescSource( krControlDescsPlain, ctrl )
      }
      if( krControlDescsLagged.size > 0 ) {
        if( verbose ) println( "XXX krControlDescsLagged NOT YET IMPLEMENTED" )
      }
    }
  }
  
  private def setControlDescSource( descs: Seq[ ControlDesc ], source: UGen ) {
      var off	= 0
      descs.foreach (desc => {
        desc.ugen				= source
        desc.ugenOutputIndex	= off
        off += desc.numOutputs
      })
  }
}

/**
 * 	@author		Hanns Holger Rutz
 *	@version	0.11, 16-Jun-09
 */
object SynthDef {
	var verbose = false
  var buildSynthDef : Option[ SynthDef ] = None
  var synthDefDir = "/tmp/"	// XXX
  
//  def apply( name: String, func: => Any ) = new SynthDef( name, func, Nil, Nil, Nil )
  def apply( name: String, rates: Seq[Any] = Nil, prependArgs: Seq[Any] = Nil, variants: Seq[Any] = Nil )( thunk: => GE ) : SynthDef = {
     def func() = thunk
     new SynthDef( name, rates, prependArgs, variants )( func )
  }
  
  def wrap( ugenGraphFunc: () => GE, rates: Seq[Any], prependArgs: Seq[Any] ) : GE = {
	if( buildSynthDef.isEmpty ) { 
	  throw new Exception( "SynthDef.wrap should be called inside a SynthDef ugenGraphFunc." )
	}
    buildSynthDef.get.buildUGenGraph( ugenGraphFunc, rates, prependArgs )
  }
  
  def writeDefFile( path: String, defs: Seq[ SynthDef ]) {
    val os	= new FileOutputStream( path )
	val dos	= new DataOutputStream( new BufferedOutputStream( os ))
 
//  try {
      dos.writeInt( 0x53436766 );		// magic cookie
      dos.writeInt( 1 );				// version
      dos.writeShort( defs.size ); 		// number of defs in file.
      defs.foreach (_.write( dos ))
//  }
//  finally {
      dos.close
//  }
  }
}