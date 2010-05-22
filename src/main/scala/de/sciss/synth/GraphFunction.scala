package de.sciss.synth

import de.sciss.scalaosc.OSCMessage

object GraphFunction {
   private var uniqueIDCnt = 0
   private val uniqueSync = new AnyRef
   private def uniqueID = {
      uniqueSync.synchronized {
         uniqueIDCnt += 1
         val result = uniqueIDCnt
         result
      }
   }
}

class GraphFunction[ T <% GE ]( thunk: => T ) {
   import GraphFunction._
   
   def play : Synth = {
      play( Server.default.defaultGroup, 0, Some(0.02f), addToHead )
   }

   def play( target: Node = Server.default.defaultGroup, outBus: Int = 0,
             fadeTime: Option[Float] = Some( 0.02f ),
             addAction: AddAction = addToHead ) : Synth = {

		val server = target.server
		val defName    = "temp_" + uniqueID // more clear than using hashCode
		val synthDef   = SynthDef( defName, GraphBuilder.wrapOut( thunk, fadeTime ))
		val synth      = new Synth( server )
		val bytes      = synthDef.toBytes
		val synthMsg   = synth.newMsg( synthDef.name, target, List( "i_out" -> outBus, "out" -> outBus ), addAction )
      synth.onEnd { server ! synthDef.freeMsg } // why would we want to accumulate the defs?
		if( bytes.remaining > (65535 / 4) ) { // "preliminary fix until full size works" (???)
			if( server.isLocal ) {
				synthDef.load( server, completion = synthMsg )
			} else {
				warn( "synthdef may have been too large to send to remote server" )
				server ! OSCMessage( "/d_recv", bytes, synthMsg )
			}
		} else {
			server ! OSCMessage( "/d_recv", bytes, synthMsg )
		}
		synth
	}
}