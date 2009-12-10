/*
 *  ServerOptions.scala
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

import _root_.scala.collection.mutable.ListBuffer

/**
 *	@author		Hanns Holger Rutz
 * 	@version	0.12, 24-Feb-09
 */
abstract class ServerOption[T]( val switch: String, val default: T ) {
  protected var valu = default
  def value : T = valu
  def value_=( newValue: T ) : ServerOption[T] = {
//    println( "switch " + switch + "; newValue " + newValue )
    valu = newValue
    this
  }
}

class ServerIntOption( override val switch: String,
                       override val default: Int )
extends ServerOption[Int]( switch, default ) {
}

class ServerBooleanOption( override val switch: String,
                           override val default: Boolean )
extends ServerOption[Boolean]( switch, default ) {
}

class ServerStringOption( override val switch: String,
                          override val default: String )
extends ServerOption[String]( switch, default ) {
}

class ServerSymbolOption( override val switch: String,
                          override val default: Symbol )
extends ServerOption[Symbol]( switch, default ) {
}

class ServerOptions {
  var initialNodeID = 1000
  
  val program				= new ServerStringOption( "", "scsynth" )
  val protocol				= new ServerSymbolOption( "", 'udp )
  val host					= new ServerStringOption( "", "127.0.0.1" )
  val port					= new ServerIntOption( "", 57100 )
  val controlBusChannels	= new ServerIntOption( "c", 4096 )
  val audioBusChannels		= new ServerIntOption( "a",  128 )
  val inputBusChannels		= new ServerIntOption( "i",    8 )
  val outputBusChannels		= new ServerIntOption( "o",    8 )
  val blockSize				= new ServerIntOption( "z",   64 )
  val hardwareBlockSize		= new ServerIntOption( "Z",    0 )
  val sampleRate			= new ServerIntOption( "S",    0 )
  val audioBuffers			= new ServerIntOption( "b", 1024 )
  val maxNodes				= new ServerIntOption( "n", 1024 )
  val maxSynthDefs			= new ServerIntOption( "d", 1024 )
  val memSize				= new ServerIntOption( "m", 8192 )
  val wireBuffers			= new ServerIntOption( "w",   64 )
  val randomSeeds			= new ServerIntOption( "r",   64 )
  val loadSynthDefs			= new ServerBooleanOption( "r", true )
  val zeroConf				= new ServerBooleanOption( "R", true )
  val maxLogins				= new ServerIntOption( "l",   64 )
  val sessionPassword		= new ServerStringOption( "p", "" )
  val inputStreamsEnabled	= new ServerStringOption( "I", "" )
  val outputStreamsEnabled	= new ServerStringOption( "O", "" )
  val machPortName			= new ServerStringOption( "M", "" )
  val inDeviceName			= new ServerStringOption( "", "" )
  val outDeviceName			= new ServerStringOption( "", "" )
  val verbosity				= new ServerIntOption( "v", 0 )
  val plugInsPath			= new ServerStringOption( "U", "" )
 
  private val switchOptions = List(
    controlBusChannels, audioBusChannels, inputBusChannels, outputBusChannels,
    blockSize, hardwareBlockSize, sampleRate, audioBuffers, maxNodes,
    maxSynthDefs, memSize, wireBuffers, randomSeeds, loadSynthDefs,
    zeroConf, maxLogins, sessionPassword, inputStreamsEnabled, outputStreamsEnabled,
    machPortName, verbosity, plugInsPath
  )
  
  def toProcessArgs : Seq[String] = {
    val result = new ListBuffer[String]()
    
    result += program.value
    protocol.value match {
      case 'tcp => result += "-t"
      case 'udp => result += "-u"
      case _ => throw new IllegalArgumentException( protocol.value.toString )
    }
    result += port.value.toString
    // XXX inDevice / outDevice
    
    switchOptions.foreach { option =>
      if( option.value != option.default ) {
        result += "-" + option.switch
        result += option.value.toString
      }
    }
    
    result
  }

  def firstPrivateBus = outputBusChannels.value + inputBusChannels.value
}
