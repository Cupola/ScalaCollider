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

/**
 *    @version	0.14, 22-Apr-10
 */
case class Synth( defName: String, server: Server, id: Int )
extends Node {
   
	def this( defName: String, server: Server = Server.default ) = this( defName, server, server.nodes.nextID )

	def newMsg( target: Node, args: Seq[ Tuple2[ Any, Float ]] = Nil, addAction: AddAction = addToHead ) =
      OSCSynthNewMessage( defName, id, addAction.id, target.id, args: _* )
}

// factory
object Synth {
   def play( defName: String, args: Seq[ Tuple2[ Any, Float ]] = Nil ) : Synth =
      head( Server.default.defaultGroup, defName, args )

   def after( target: Node, defName: String, args: Seq[ Tuple2[ Any, Float ]] = Nil ) = {
	   val synth = new Synth( defName, target.server );
      synth.server ! synth.newMsg( target, args, addAfter )
      synth
	}
 
   def before( target: Node, defName: String, args: Seq[ Tuple2[ Any, Float ]] = Nil ) = {
	   val synth = new Synth( defName, target.server )
      synth.server ! synth.newMsg( target, args, addBefore )
      synth
	}
 
	def head( target: Group, defName: String, args: Seq[ Tuple2[ Any, Float ]] = Nil ) = {
	   val synth = new Synth( defName, target.server )
      synth.server ! synth.newMsg( target, args, addToHead )
      synth
	}

	def tail( target: Group, defName: String, args: Seq[ Tuple2[ Any, Float ]] = Nil ) = {
	   val synth = new Synth( defName, target.server )
      synth.server ! synth.newMsg( target, args, addToTail )
      synth
	}
 
	def replace( target: Node, defName: String, args: Seq[ Tuple2[ Any, Float ]] = Nil ) = {
	   val synth = new Synth( defName, target.server )
      synth.server ! synth.newMsg( target, args, addReplace )
      synth
	}
  
//  def newPaused = {
//    new Synth( "schoko", new Server( "lala", new NetAddr, new ServerOptions, 0 ), 0 );
// }
}