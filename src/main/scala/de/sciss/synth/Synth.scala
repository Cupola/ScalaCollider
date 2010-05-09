/*
 *  Synth.scala
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

/**
 *    @version	0.15, 09-May-10
 */
case class Synth( server: Server, id: Int )
extends Node {
   private var defNameVar = ""

	def this( server: Server = Server.default ) = this( server, server.nodes.nextID )

	def newMsg( defName: String, target: Node, args: Seq[ ControlSetMap ] = Nil, addAction: AddAction = addToHead ) = {
      defNameVar = defName
      OSCSynthNewMessage( defName, id, addAction.id, target.id, args: _* )
   }

   def defName = defNameVar

   override def toString = "Synth(" + server + "," + id +
      (if( defNameVar != "" ) ") : <" + defNameVar + ">" else ")")
}

// factory
object Synth {
   def play( defName: String, args: Seq[ ControlSetMap ] = Nil, target: Node = Server.default.defaultGroup,
             addAction: AddAction = addToHead ) = {
      val synth = new Synth( target.server )
      synth.server ! synth.newMsg( defName, target, args, addAction )
      synth
   }

   def after( target: Node, defName: String, args: Seq[ ControlSetMap ] = Nil ) : Synth =
      play( defName, args, target, addAfter )
 
   def before( target: Node, defName: String, args: Seq[ ControlSetMap ] = Nil ) : Synth =
      play( defName, args, target, addBefore )

	def head( target: Group, defName: String, args: Seq[ ControlSetMap ] = Nil ) : Synth =
      play( defName, args, target, addToHead )

	def tail( target: Group, defName: String, args: Seq[ ControlSetMap ] = Nil ) : Synth =
      play( defName, args, target, addToTail )

	def replace( target: Node, defName: String, args: Seq[ ControlSetMap ] = Nil ) : Synth =
      play( defName, args, target, addReplace )

   def apply( server: Server = Server.default ) : Synth = apply( server, server.nodes.nextID )
}