/*
 *  Synth.scala
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

import _root_.de.sciss.scalaosc.OSCMessage

/**
 * 	@author		Hanns Holger Rutz
 *	@version	0.12, 16-Jun-09
 */
// case class Synth( defName: String, override val server: Server, override val id: Int )
class Synth( val defName: String, override val server: Server, override val id: Int )
extends Node( server, id )
{
	def this( defName: String, server: Server = Server.default ) = this( defName, server, server.nodes.nextID )

	def newMsg( target: => Node, args: Seq[Tuple2[String,Float]] = Nil, addAction: Symbol = 'addToHead ) : OSCMessage = {
		val argsA = new Array[ Any ]( (args.size << 1) + 4 )
		argsA( 0 ) = defName
		argsA( 1 ) = id
		argsA( 2 ) = Nodes.actionNumberFor( addAction )
		argsA( 3 ) = target.id
		var i = 4
		args.foreach { pair => {
			argsA( i ) = pair._1
			i += 1
			argsA( i ) = pair._2
			i += 1
		}}
		OSCMessage( "/s_new", argsA:_* )
	}

	override def toString = "Synth( \"" + defName + "\" : " + id + " )"
}

// factory
object Synth {
    def spawn( defName: String, args: Seq[Tuple2[String,Float]] = Nil ): Synth = {
      head( new Group( Server.default, 0 ), defName, args )
	}

    def after( target: => Node, defName: String, args: Seq[Tuple2[String,Float]] = Nil ): Synth = {
	  val synth = new Synth( defName, target.server );
      synth.server.sendMsg( synth.newMsg( target, args, 'addAfter ));
      synth;
	}
 
    def before( target: => Node, defName: String, args: Seq[Tuple2[String,Float]] = Nil ): Synth = {
	  val synth = new Synth( defName, target.server )
      synth.server.sendMsg( synth.newMsg( target, args, 'addBefore ))
      synth
	}
 
	def head( target: => Node, defName: String, args: Seq[Tuple2[String,Float]] = Nil ): Synth = { 
	  val synth = new Synth( defName, target.server )
      synth.server.sendMsg( synth.newMsg( target, args, 'addToHead ))
      synth
	}

	def tail( target: => Node, defName: String, args: Seq[Tuple2[String,Float]] = Nil ): Synth = { 
	  val synth = new Synth( defName, target.server )
      synth.server.sendMsg( synth.newMsg( target, args, 'addToTail ))
      synth
	}
 
	def replace( target: => Node, defName: String, args: Seq[Tuple2[String,Float]] = Nil ): Synth = {
	  val synth = new Synth( defName, target.server )
      synth.server.sendMsg( synth.newMsg( target, args, 'addReplace ))
      synth
	}
  
//  def newPaused = {
//    new Synth( "schoko", new Server( "lala", new NetAddr, new ServerOptions, 0 ), 0 );
// }
}