/*
 *  ServerStatusPanel.scala
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
package de.sciss.tint.sc.gui

import _root_.java.awt.{ Color, Component, Container, Dimension, Graphics, Font }
import _root_.javax.swing.{ BorderFactory, BoxLayout, JComponent, JFrame, JLabel, JPanel,
	WindowConstants }
import _root_.javax.swing.SwingConstants._
import _root_.javax.swing.event.{ AncestorEvent, AncestorListener }

import _root_.de.sciss.tint.sc.{ Server }

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.10, 26-Nov-09
 */
class ServerStatusPanel( server: Server ) extends JPanel {
	private val lbCPU		= new CPUIndicator
	private val lbNumUGens	= new JLabel
	private val lbNumSynths	= new JLabel
	private val lbNumGroups	= new JLabel
	private val lbNumDefs	= new JLabel
		
	// ---- constructor ----
	{
		setLayout( new BoxLayout( this, BoxLayout.X_AXIS ))
//		add( new JLabel( "avg CPU : ", RIGHT ))
//		add( lbCntAvgCPU )
//		add( new JLabel( "peak CPU : ", RIGHT ))
//		add( lbCntPeakCPU )
		lbCPU.setPreferredSize( new Dimension( 80, 16 ))
		add( lbCPU )
		add( new JLabel( " G : " ))
		lbNumGroups.setPreferredSize( new Dimension( 40, 16 ))
		add( lbNumGroups )
		add( new JLabel( "S : " ))
		lbNumSynths.setPreferredSize( new Dimension( 40, 16 ))
		add( lbNumSynths )
		add( new JLabel( "U : " ))
		lbNumUGens.setPreferredSize( new Dimension( 40, 16 ))
		add( lbNumUGens )
		add( new JLabel( "D : " ))
		lbNumDefs.setPreferredSize( new Dimension( 40, 16 ))
		add( lbNumDefs )
		
		setBorder( BorderFactory.createEmptyBorder( 2, 2, 2, 2 ))
		val fntGUI = new Font( "Lucida Grande", Font.PLAIN, 9 )
		setDeepFont( this, fntGUI )
		
		addAncestorListener( new AncestorListener {
			def ancestorAdded( e: AncestorEvent ) {
				addListener
				updateCounts	
			}
			
			def ancestorRemoved( e: AncestorEvent ) {
				removeListener
				updateCounts			
			}
			
			def ancestorMoved( e: AncestorEvent ) {}
		})
	}
	
	def makeWindow: JFrame = {
		val frame = new JFrame( "Counts" )
		frame.setResizable( false )
		frame.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
		frame.getContentPane.add( this )
		frame.pack()
		frame.setVisible( true )
		frame
	}
	
	private var addedListener = false
	
	private def addListener {
		if( !addedListener ) {
			addedListener = true
			server.addListener( serverUpdate )
		}
	}
	
	private def removeListener {
		if( !addedListener ) {
			server.removeListener( serverUpdate )
			addedListener = false
		}
	}
	
//	private var srv: Option[ Server ] = None
//	def server = srv
//	def server_=( newServer: Option[ Server ]) {
//		if( srv == newServer ) return
//		val wasAdded = addedListener
//		removeListener
//		srv = newServer
//		if( isShowing ) {
//			updateCounts
//			if( srv.isDefined ) addListener
//		}
//	}
	
	private def serverUpdate( s: Server, what: Symbol ) {
		if( isShowing && what == 'counts ) updateCounts 
	}
	
	private def updateCounts {
		if( isShowing ) {
			if( server.isRunning ) {
				val counts = server.counts
				lbCPU.update( counts.avgCPU / 100, counts.peakCPU / 100 )
				lbNumUGens.setText( counts.numUGens.toString )
				lbNumSynths.setText( counts.numSynths.toString )
				lbNumGroups.setText( counts.numGroups.toString )
				lbNumDefs.setText( counts.numDefs.toString )
			}
			else clearCounts
		} else clearCounts
	}
	
	private def clearCounts {
		lbNumUGens.setText( null )
		lbNumSynths.setText( null )
		lbNumGroups.setText( null )
		lbNumDefs.setText( null )
	}
	
	private def setDeepFont( c: Component, fnt: Font ) {
		c.setFont( fnt )
		c match {
			case con: Container => con.getComponents.foreach( setDeepFont( _, fnt ))
			case _ =>
		}
	}
	
	private class CPUIndicator extends JComponent {
		private var avgCPU = 0f
		private var peakCPU = 0f
		private var avgW = 0
		private var peakX = 0
		
		def update( newAvgCPU: Float, newPeakCPU: Float ) {
			avgCPU  = newAvgCPU
			peakCPU = newPeakCPU
			val oldAvgW = avgW
			val oldPeakX = peakX
			updateScreenCoords
			if( (oldAvgW != avgW) || (oldPeakX != peakX) ) repaint() // could use dirty rect
		}
		
		override def paintComponent( g: Graphics ) {
//			val g2 = g.asInstanceOf[ Graphics2D ]
			g.setColor( Color.black )
			val w = getWidth
			val h = getHeight
//			g.drawRect(  0, 0, w - 1, h - 1 )
			g.fillRect(  0, 0, w, h )
			updateScreenCoords
			g.setColor( Color.yellow /* Color.blue */)
			g.fillRect( 1, 1, avgW, h - 2 )
			g.drawLine( peakX, 1, peakX, h - 2 )
		}
		
		private def updateScreenCoords {
			val w = getWidth
			avgW  = (avgCPU  * (w - 2)).toInt // + 1
			peakX = (peakCPU * (w - 2)).toInt + 1
		}
	}
}
