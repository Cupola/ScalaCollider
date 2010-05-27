package de.sciss.synth

import concurrent.SyncVar
import actors.{ Channel, DaemonActor, Future, InputChannel }

/*
 *    FutureActor in scala.actors is not very accessible...
 *    We need our own implementation of Future is seems
 */
private[synth] class FutureActor[ T ]( fun: SyncVar[ T ] => Unit, channel: Channel[ T ])
extends Future[ T ] with DaemonActor {
   @volatile private var v: Option[T] = None

   private case object Eval

   def isSet = !v.isEmpty

   def apply(): T = {
      if( v.isEmpty ) error( "Thread-based operations not supported" )
      v.get
   }

   def respond( k: T => Unit ) {
      v.map( k( _ )) getOrElse {
         val fut = this !! Eval
         fut.inputChannel.react {
            case _ => k( v.get )
         }
      }
   }

   def inputChannel: InputChannel[ T ] = channel

   def act() {
      val syncVar = new SyncVar[ T ]

      { fun( syncVar )} andThen {
         val syncVal = syncVar.get
         v = Some( syncVal )
         channel ! syncVal
         loop { react {
            case Eval => reply()
         }}
      }
   }
}
