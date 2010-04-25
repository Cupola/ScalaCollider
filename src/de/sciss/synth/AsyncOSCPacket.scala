package de.sciss.synth

import de.sciss.scalaosc.{ OSCMessage, OSCPacket }

trait AsyncOSCPacket {
   p: OSCPacket =>

   def replyMessage: OSCMessage
   def packet: OSCPacket = this
}