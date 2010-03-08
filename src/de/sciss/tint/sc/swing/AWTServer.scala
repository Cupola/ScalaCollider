package de.sciss.tint.sc.swing

import java.awt.{ EventQueue }
import de.sciss.tint.sc.{ Server, ServerOptions }

/**
 *    A minimal subclass that uses the java.awt.EventThread for dispatch
 */
class AWTServer( val name: String, val options: ServerOptions = new ServerOptions, val clientID: Int = 0 )
extends Server {

   protected def invokeOnMainThread( task: Runnable ) {
      EventQueue.invokeLater( task )
   }
}