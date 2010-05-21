package de.sciss.synth.osc

import de.sciss.scalaosc.OSCMessage
import de.sciss.synth.Server

trait OSCHandler {
   private[synth] def handle( msg: OSCMessage ) : Boolean
   private[synth] def removed : Unit
}

object OSCResponder {
   def add( handler: PartialFunction[ OSCMessage, Unit ], server: Server = Server.default ) : OSCResponder =
      new Impl( server, handler, false ).add

   def once( handler: PartialFunction[ OSCMessage, Unit ], server: Server = Server.default ) : OSCResponder =
      new Impl( server, handler, true ).add

   def apply( handler: PartialFunction[ OSCMessage, Unit ], server: Server = Server.default ): OSCResponder =
      new Impl( server, handler, false )

   private class Impl( val server: Server, handler: PartialFunction[ OSCMessage, Unit ], once: Boolean )
   extends OSCResponder {
      def add     = { server.addResponder( this ); this }
      def remove  = { server.removeResponder( this ); this }

      private[synth] def handle( msg: OSCMessage ) : Boolean = {
         val handled = handler.isDefinedAt( msg )
         if( handled ) try {
            handler( msg )
         } catch { case e => e.printStackTrace() }
         once
      }

      private[synth] def removed {}
   }
}

trait OSCResponder extends OSCHandler {
   def server: Server
   def add : OSCResponder
   def remove : OSCResponder
}