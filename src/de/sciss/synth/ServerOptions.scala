/*
 *  ServerOptions.scala
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

import collection.mutable.ListBuffer

/**
 *    @author		Hanns Holger Rutz
 * 	@version    0.13, 22-Feb-10
 */
class ServerOption[T]( val switch: String, val default: T ) {
   protected var valu = default
   def value : T = valu
   def value_=( newValue: T ) : ServerOption[T] = {
//    println( "switch " + switch + "; newValue " + newValue )
      valu = newValue
      this
   }

   def stringValue: String = valu.toString
}

class ServerIntOption( sw: String, defau: Int )
extends ServerOption[Int]( sw, defau )

class ServerBooleanOption( sw: String, defau: Boolean )
extends ServerOption[Boolean]( sw, defau ) {
   override def stringValue: String = if( valu ) "1" else "0"
}

class ServerStringOption( sw: String, defau: String )
extends ServerOption[String]( sw, defau )

//class ServerSymbolOption( sw: String, defau: Symbol )
//extends ServerOption[Symbol]( sw, defau )

class ServerOptions {
   var initialNodeID = 1000
  
   val programPath		      = new ServerStringOption( "", "scsynth" )
   val controlBusChannels	   = new ServerIntOption( "c", 4096 )
   val audioBusChannels		   = new ServerIntOption( "a",  128 )
   val outputBusChannels		= new ServerIntOption( "o",    8 )
   val blockSize				   = new ServerIntOption( "z",   64 )
   val sampleRate			      = new ServerIntOption( "S",    0 )
   val audioBuffers		   	= new ServerIntOption( "b", 1024 )
   val maxNodes				   = new ServerIntOption( "n", 1024 )
   val maxSynthDefs		   	= new ServerIntOption( "d", 1024 )
   val memSize		   	   	= new ServerIntOption( "m", 8192 )
   val wireBuffers		   	= new ServerIntOption( "w",   64 )
   val randomSeeds			   = new ServerIntOption( "r",   64 )
   val loadSynthDefs			   = new ServerBooleanOption( "D", true )
   val machPortName			   = new ServerStringOption( "M", "" )
   val verbosity			   	= new ServerIntOption( "v", 0 )
   val plugInsPath			   = new ServerStringOption( "U", "" )

   // realtime only
   val host					      = new ServerStringOption( "", "127.0.0.1" )
   val port					      = new ServerIntOption( "", 57100 )
//   val protocol				   = new ServerSymbolOption( "", 'udp )
   val protocol				   = new ServerStringOption( "", "udp" )
   val inputStreamsEnabled	   = new ServerStringOption( "I", "" )
   val outputStreamsEnabled	= new ServerStringOption( "O", "" )
   val inDeviceName		   	= new ServerStringOption( "", "" )
   val outDeviceName		   	= new ServerStringOption( "", "" )
   val inputBusChannels		   = new ServerIntOption( "i",    8 )
   val hardwareBlockSize		= new ServerIntOption( "Z",    0 )
   val zeroConf			   	= new ServerBooleanOption( "R", true )
   val maxLogins				   = new ServerIntOption( "l",   64 )
   val sessionPassword		   = new ServerStringOption( "p", "" )

   // nonrealtime only
   val nrtCmdPath    		   = new ServerStringOption( "", "" )
   val nrtInputPath    		   = new ServerStringOption( "", "_" )
   val nrtOutputPath   		   = new ServerStringOption( "", "" )
   val nrtHeaderFormat   	   = new ServerStringOption( "", "AIFF" )
   val nrtSampleFormat   	   = new ServerStringOption( "", "float32" )

   private val switchOptions = List(
      controlBusChannels, audioBusChannels, outputBusChannels,
      blockSize, audioBuffers, maxNodes,
      maxSynthDefs, memSize, wireBuffers, randomSeeds, loadSynthDefs,
      machPortName, verbosity, plugInsPath
   )
  
   private val rtSwitchOptions = switchOptions ::: List(
      sampleRate, inputStreamsEnabled, outputStreamsEnabled, inputBusChannels,
      hardwareBlockSize, zeroConf, maxLogins, sessionPassword
   )

   def toRealtimeArgs : List[ String ] = {
      val result = new ListBuffer[String]()
    
      result += programPath.stringValue
      protocol.value match {
         case "tcp" => result += "-t"
         case "udp" => result += "-u"
         case _ => error( protocol.stringValue )
      }
      result += port.stringValue
      if( inDeviceName.value == outDeviceName.value ) {
         if( inDeviceName.value != "" ) {
            result += "-H"
            result += inDeviceName.stringValue
         }
      } else {
         result += "-H"
         result += inDeviceName.stringValue
         result += outDeviceName.stringValue
      }
    
      rtSwitchOptions.foreach { option =>
         if( option.value != option.default ) {
            result += "-" + option.switch
            result += option.stringValue
         }
      }
    
      result.toList
   }

   def toNonRealtimeArgs : List[ String ] = {
      val result = new ListBuffer[String]()

      result += programPath.stringValue
      result += "-N"
      result += nrtCmdPath.stringValue
      result += nrtInputPath.stringValue
      result += nrtOutputPath.stringValue
      result += sampleRate.stringValue
      result += nrtHeaderFormat.stringValue
      result += nrtSampleFormat.stringValue

      switchOptions.foreach { option =>
         if( option.value != option.default ) {
            result += "-" + option.switch
            result += option.stringValue
         }
      }

      result.toList
   }

   def firstPrivateBus = outputBusChannels.value + inputBusChannels.value
}
