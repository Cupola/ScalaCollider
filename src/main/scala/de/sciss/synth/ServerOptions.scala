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
import io.{ AudioFileType, SampleFormat }
import java.io.File
import de.sciss.osc.{ OSCTransport, TCP, UDP }
import java.net.{DatagramSocket, ServerSocket}

/**
 * 	@version    0.14, 27-May-10
 */
trait ServerOptionsLike {
   def programPath:           String
   def controlBusChannels:    Int
   def audioBusChannels:      Int
   def outputBusChannels:     Int
   def blockSize:             Int
   def sampleRate:            Int
   def audioBuffers:          Int
   def maxNodes:              Int
   def maxSynthDefs:          Int
   def memorySize:            Int
   def wireBuffers:           Int
   def randomSeeds:           Int
   def loadSynthDefs:         Boolean
   def machPortName:          Option[ (String, String) ]
   def verbosity:             Int
   def plugInsPaths:          List[ String ]
   def restrictedPath:        Option[ String ]
   def memoryLocking:         Boolean

//   // client only
//   def clientID:              Int
//   def nodeIDOffset:          Int

   // realtime only
   def host:                  String
   def port:                  Int
   def transport:             OSCTransport
   def inputStreamsEnabled:   Option[ String ]
   def outputStreamsEnabled:  Option[ String ]
   def deviceName:            Option[ String ]
   def deviceNames:           Option[ (String, String) ]
   def inputBusChannels:      Int
   def hardwareBlockSize:     Int
   def zeroConf:              Boolean
   def maxLogins:             Int
   def sessionPassword:       Option[ String ]

   // nonrealtime only
   def nrtCommandPath:        String
   def nrtInputPath:          Option[ String ]
   def nrtOutputPath:         String
   def nrtHeaderFormat:       AudioFileType
   def nrtSampleFormat:       SampleFormat

   def toRealtimeArgs:        List[ String ]
//   def toNonRealtimeArgs:     List[ String ]

   def firstPrivateBus: Int = outputBusChannels + inputBusChannels
}

abstract class ServerOptions extends ServerOptionsLike

object ServerOptionsBuilder {
   private def toRealtimeArgs( o: ServerOptionsLike ): List[ String ] = {
      val result = new ListBuffer[ String ]()

      result += o.programPath
      o.transport match {
         case TCP => {
            result += "-t"
            result += o.port.toString
         }
         case UDP => {
            result += "-u"
            result += o.port.toString
         }
      }
      if( o.controlBusChannels != 4096 ) {
         result += "-c"
         result += o.controlBusChannels.toString
      }
      if( o.audioBusChannels != 128 ) {
         result += "-a"
         result += o.audioBusChannels.toString
      }
      if( o.inputBusChannels != 8 ) {
         result += "-i"
         result += o.inputBusChannels.toString
      }
      if( o.outputBusChannels != 8 ) {
         result += "-o"
         result += o.outputBusChannels.toString
      }
      if( o.blockSize != 64 ) {
          result += "-z"
          result += o.blockSize.toString
      }
      if( o.hardwareBlockSize != 0 ) {
          result += "-Z"
          result += o.hardwareBlockSize.toString
      }
      if( o.sampleRate != 0 ) {
         result += "-S"
         result += o.sampleRate.toString
      }
      if( o.audioBuffers != 1024 ) {
         result += "-b"
         result += o.audioBuffers.toString
      }
      if( o.maxNodes != 1024 ) {
         result += "-n"
         result += o.maxNodes.toString
      }
      if( o.maxSynthDefs != 1024 ) {
         result += "-d"
         result += o.maxSynthDefs.toString
      }
      if( o.memorySize != 8192 ) {
         result += "-m"
         result += o.memorySize.toString
      }
      if( o.wireBuffers != 64 ) {
         result += "-w"
         result += o.wireBuffers.toString
      }
      if( o.randomSeeds != 64 ) {
         result += "-r"
         result += o.randomSeeds.toString
      }
      if( !o.loadSynthDefs ) {
         result += "-D"
         result += "0"
      }
      if( !o.zeroConf ) {
         result += "-R"
         result += "0"
      }
      if( o.maxLogins != 64 ) {
         result += "-l"
         result += o.maxLogins.toString
      }
      o.sessionPassword.foreach( pwd => {
         result += "-p"
         result += pwd
      })
      o.inputStreamsEnabled.foreach( stream => {
         result += "-I"
         result += stream
      })
      o.outputStreamsEnabled.foreach( stream => {
         result += "-O"
         result += stream
      })
      o.machPortName.foreach( tup => {
         result += "-M"
         result += tup._1
         result += tup._2
      })
      o.deviceNames.foreach( tup => {
         val (inDev, outDev) = tup
         result += "-H"
         result += tup._1
         result += tup._2
      })
      o.deviceName.foreach( n => {
         result += "-H"
         result += n
      })
      if( o.verbosity != 0 ) {
         result += "-v"
         result += o.verbosity.toString
      }
      if( o.plugInsPaths.nonEmpty ) {
         result += "-U"
         result += o.plugInsPaths.mkString( ":" )
      }
      o.restrictedPath.foreach( path => {
         result += "-P"
         result += path
      })
      if( o.memoryLocking ) {
         result += "-L"
      }

      result.toList
   }
}

class ServerOptionsBuilder extends ServerOptionsLike {
   var programPath:           String = new File( System.getenv( "SC_HOME" ), "scsynth" ).getAbsolutePath
   var controlBusChannels:    Int                        = 4096
   var audioBusChannels:      Int                        = 128
   var outputBusChannels:     Int                        = 8
   var blockSize:             Int                        = 64
   var sampleRate:            Int                        = 0
   var audioBuffers:          Int                        = 1024
   var maxNodes:              Int                        = 1024
   var maxSynthDefs:          Int                        = 1024
   var memorySize:            Int                        = 8192
   var wireBuffers:           Int                        = 64
   var randomSeeds:           Int                        = 64
   var loadSynthDefs:         Boolean                    = true
   var machPortName:          Option[ (String, String) ] = None
   var verbosity:             Int                        = 0
   var plugInsPaths:          List[ String ]             = Nil
   var restrictedPath:        Option[ String ]           = None
   var memoryLocking:         Boolean                    = false

//   // client only
//   var clientID:              Int                        = 0
//   var nodeIDOffset:          Int                        = 1000

   // realtime only
   var host:                  String                     = "127.0.0.1"
   var port:                  Int                        = 57110
   var transport:             OSCTransport               = UDP
   var inputStreamsEnabled:   Option[ String ]           = None
   var outputStreamsEnabled:  Option[ String ]           = None

   private var deviceNameVar:  Option[ String ] = None
   private var deviceNamesVar: Option[ (String, String) ] = None

   def deviceName:            Option[ String ] = deviceNameVar
   def deviceNames:           Option[ (String, String) ] = deviceNamesVar
   def deviceName_=( value: Option[ String ]) {
      deviceNameVar = value
      if( value.isDefined ) deviceNamesVar = None
   }
   def deviceNames_=( value: Option[ (String, String) ]) {
      deviceNamesVar = value
      if( value.isDefined ) deviceNameVar = None
   }

   var inputBusChannels:      Int                        = 8
   var hardwareBlockSize:     Int                        = 0
   var zeroConf:              Boolean                    = true
   var maxLogins:             Int                        = 64
   var sessionPassword:       Option[ String ]           = None

   // nonrealtime only
   var nrtCommandPath:        String                     = ""
   var nrtInputPath:          Option[ String ]           = None
   var nrtOutputPath:         String                     = ""
   var nrtHeaderFormat:       AudioFileType              = AudioFileType.AIFF
   var nrtSampleFormat:       SampleFormat               = SampleFormat.Float

   def toRealtimeArgs : List[ String ] = ServerOptionsBuilder.toRealtimeArgs( this )
   def build : ServerOptions = new Impl(
      programPath, controlBusChannels, audioBusChannels, outputBusChannels, blockSize, sampleRate, audioBuffers,
      maxNodes, maxSynthDefs, memorySize, wireBuffers, randomSeeds, loadSynthDefs, machPortName, verbosity,
      plugInsPaths, restrictedPath, memoryLocking, host, port, transport, inputStreamsEnabled, outputStreamsEnabled,
      deviceNames, deviceName, inputBusChannels, hardwareBlockSize, zeroConf, maxLogins, sessionPassword,
      nrtCommandPath,
      nrtInputPath, nrtOutputPath, nrtHeaderFormat, nrtSampleFormat )

//   def toNonRealtimeArgs : List[ String ] = {
//      val result = new ListBuffer[String]()
//
//      result += programPath.stringValue
//      result += "-N"
//      result += nrtCmdPath.stringValue
//      result += nrtInputPath.stringValue
//      result += nrtOutputPath.stringValue
//      result += sampleRate.stringValue
//      result += nrtHeaderFormat.stringValue
//      result += nrtSampleFormat.stringValue
//
//      switchOptions.foreach { option =>
//         if( option.value != option.default ) {
//            result += "-" + option.switch
//            result += option.stringValue
//         }
//      }
//
//      result.toList
//   }

   private class Impl( val programPath: String, val controlBusChannels: Int, val audioBusChannels: Int,
                       val outputBusChannels: Int, val blockSize: Int, val sampleRate: Int, val audioBuffers: Int,
                       val maxNodes: Int, val maxSynthDefs: Int, val memorySize: Int, val wireBuffers: Int,
                       val randomSeeds: Int, val loadSynthDefs: Boolean, val machPortName: Option[ (String, String) ],
                       val verbosity: Int, val plugInsPaths: List[ String ], val restrictedPath: Option[ String ],
                       val memoryLocking: Boolean, val host: String, val port: Int, val transport: OSCTransport,
                       val inputStreamsEnabled: Option[ String ], val outputStreamsEnabled: Option[ String ],
                       val deviceNames: Option[ (String, String) ], val deviceName: Option[ String ],
                       val inputBusChannels: Int,
                       val hardwareBlockSize: Int, val zeroConf: Boolean, val maxLogins: Int,
                       val sessionPassword: Option[ String ], val nrtCommandPath: String,
                       val nrtInputPath: Option[ String ],
                       val nrtOutputPath: String, val nrtHeaderFormat: AudioFileType,
                       val nrtSampleFormat: SampleFormat )
   extends ServerOptions {
      def toRealtimeArgs : List[ String ] = ServerOptionsBuilder.toRealtimeArgs( this )
   }
}
