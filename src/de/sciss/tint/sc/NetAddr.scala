/*
 *  NetAddr.scala
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

//import _root_.de.sciss.net.OSCBundle
import _root_.de.sciss.scalaosc.{ OSCClient, OSCBundle, OSCMessage }
import _root_.java.net.InetSocketAddress

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.11, 24-Nov-09
 */
case class NetAddr( host: String, port: Int, protocol: Symbol ) {
	private val inetSocketAddr	= new InetSocketAddress( host, port )
	private val inetAddr			= inetSocketAddr.getAddress
	private val isLoopback		= inetAddr.isLoopbackAddress
	private val client			= OSCClient( protocol, 0, isLoopback )  
	val isLocal					= isLoopback || inetAddr.isSiteLocalAddress
  
	client.target_=( inetSocketAddr )
  
	def sendMsg( name: String, msg: Any* ) {
		client.send( OSCMessage( name, msg:_* ))
	}

//  def listSendMsg( msg: Seq[Any] ) {
//    val oscMsg = new OSCMessage( msg.head.toString, msg.drop( 1 ).map(_.asInstanceOf[AnyRef]).toArray )
//    client.send( oscMsg )
//  }

//  def listSendBundle( time: Option[Double], bndl: Seq[ Seq[ Any ]]) {
//    val oscBndl = if( time.isDefined ) new de.sciss.net.OSCBundle( time.get ) else new de.sciss.net.OSCBundle()
//    bndl.foreach (msg => {
//      oscBndl.addPacket( new OSCMessage( msg.head.toString, msg.drop( 1 ).map(_.asInstanceOf[AnyRef]).toArray ))
//    })
//    client.send( oscBndl )
//  }

	def sendBundle( time: Option[Double], msgs: OSCMessage* ) {
		if( time.isDefined ) println( "OSCBundle:sendBundle timetag not yet supported" )
//		val oscBndl = if( time.isDefined ) new de.sciss.net.OSCBundle( time.get ) else new de.sciss.net.OSCBundle()
//		bndl.foreach (msg => {
//			oscBndl.addPacket( new OSCMessage( msg.head.toString, msg.drop( 1 ).map(_.asInstanceOf[AnyRef]).toArray ))
//		})
		client.send( OSCBundle( msgs:_* ))
	}
  
	def connect { client.connect }
}