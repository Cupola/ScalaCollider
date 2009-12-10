/*
 *  Routine.scala
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

import _root_.scala.actors.Actor
import _root_.scala.actors.Actor.{ loop, self }
import _root_.scala.collection.mutable.PriorityQueue
import _root_.scala.math._

// CCC
//import _root_.scala.continuations.cps
//import _root_.scala.continuations.ControlContext.{ shift, reset, suspendable }

case class Reschedule( delta: Long, who: Actor )
case object Awake
//case class Scheduled( when: Long, what: Actor ) extends Ordered[Scheduled] {
//    def compare( that: Scheduled ) = when.compare( that.when )
//}
class Scheduled( val when: Long, val what: Actor ) {
}
object Scheduled extends Ordering[ Scheduled ]{
	def apply( when: Long, what: Actor ) : Scheduled = new Scheduled( when, what )
    def compare( x: Scheduled, y: Scheduled ) = x.when.compare( y.when )
}

object Clock {
  var default: Clock = _
//  var default: Scheduler = new Scheduler
//  default.start
}

class Clock extends Actor {
    var verbose = false

    private val queue = new PriorityQueue[Scheduled]()( Scheduled )
    
	def act {
		loop {
            val now = System.currentTimeMillis
            if( verbose ) {
                println( "Entering react at " + now + " ; " + Thread.currentThread.hashCode )
            }
			react {
				case r: Reschedule => {
					if( verbose ) {
                        println( "Received reschedule " + r.delta )
                    }
                    queue += Scheduled( now + max( 0, r.delta ), r.who )
                    while( !queue.isEmpty ) {
                        val x = queue.dequeue
                        if( verbose ) {
                            println( "Dequeued " + x.when )
                        }
                        if( x.when > now ) {
                            if( verbose ) {
                                println( "Sleeping for " + (x.when - now) )
                        }
                            Thread.sleep( x.when - now )
                        }
                        if( verbose ) {
                            println( "Awaking actor : " + r.who )
                        }
                        x.what ! Awake
                    }
                    if( verbose ) println( "Queue done" )
				}

                case _ => println( "Wooooo" )
			}
        }
	}
}

// CCC
//class Routine[A,C]( val clock: Clock, ctx: => (A @cps[ A, C ])) extends Actor {
//  def act {
//    reset( ctx )
//  }
//  
//  private def proceed[A, B](fun: PartialFunction[A, Unit] => Nothing):
//    PartialFunction[A, B] => B @cps[Unit, Unit] =
//      (caseBlock: PartialFunction[A, B]) =>
//           shift[B, Unit, Unit]((k: B => Unit) => fun(caseBlock andThen k))
//
//  def sleep(delta: Long): Unit @suspendable = {
//    clock ! Reschedule( delta, self )
//    proceed( react ) { case Awake => /* println("Awoken")*/ }
//  }
//}
