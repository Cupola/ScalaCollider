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

import java.awt.{ Color, Component, Container, Dimension, FlowLayout, Font,
                 Graphics, Toolkit }
import javax.swing.{ BorderFactory, Box, BoxLayout, ImageIcon, JComponent,
                    JFrame, JLabel, JPanel, SwingConstants, WindowConstants }
import SwingConstants._
import javax.swing.event.{ AncestorEvent, AncestorListener }
import scala.math._

import de.sciss.tint.sc.{ OSCStatusReplyMessage, Server }

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.11, 12-Jan-10
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
//		setLayout( new FlowLayout( FlowLayout.TRAILING, 4, 4 ))
//		add( new JLabel( "avg CPU : ", RIGHT ))
//		add( lbCntAvgCPU )
//		add( new JLabel( "peak CPU : ", RIGHT ))
//		add( lbCntPeakCPU )
//		lbCPU.setPreferredSize( new Dimension( 80, 16 ))

//        val tk = Toolkit.getDefaultToolkit
        val icnGroup = new ImageIcon( getClass.getResource( "path_group_16.png" ))
        val icnSynth = new ImageIcon( getClass.getResource( "path_synth_16.png" ))
        val icnUGen  = new ImageIcon( getClass.getResource( "path_ugen_16.png" ))
        val icnDef   = new ImageIcon( getClass.getResource( "path_def_16.png" ))

        def flushImages {
          icnGroup.getImage.flush
          icnSynth.getImage.flush
          icnUGen.getImage.flush
          icnDef.getImage.flush
        }

        def addS( c: Component, gap: Int = 4 ) {
          add( c )
          add( Box.createHorizontalStrut( gap ))
        }

		addS( lbCPU, 8 )
		addS( new JLabel( icnGroup ))
		lbNumGroups.setPreferredSize( new Dimension( 40, 16 ))
		addS( lbNumGroups )
		addS( new JLabel( icnSynth ))
		lbNumSynths.setPreferredSize( new Dimension( 40, 16 ))
		addS( lbNumSynths )
		addS( new JLabel( icnUGen ))
		lbNumUGens.setPreferredSize( new Dimension( 40, 16 ))
		addS( lbNumUGens )
		addS( new JLabel( icnDef ))
		lbNumDefs.setPreferredSize( new Dimension( 40, 16 ))
		add( lbNumDefs )
		
		setBorder( BorderFactory.createEmptyBorder( 2, 2, 2, 2 ))
		val fntGUI = new Font( "Lucida Grande", Font.PLAIN, 9 )
		setDeepFont( this, fntGUI )
		
		addAncestorListener( new AncestorListener {
			def ancestorAdded( e: AncestorEvent ) {
				addListener
//				updateCounts
			}
			
			def ancestorRemoved( e: AncestorEvent ) {
				removeListener
                clearCounts
                flushImages
//				updateCounts
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
	
	private def serverUpdate( msg: AnyRef ) : Unit = msg match {
      case Server.Counts( cnt ) if isShowing => updateCounts( cnt )
      case Server.Offline => clearCounts
	}
	
	private def updateCounts( cnt: OSCStatusReplyMessage ) {
		lbCPU.update( cnt.avgCPU / 100, cnt.peakCPU / 100 )
		lbNumUGens.setText( cnt.numUGens.toString )
		lbNumSynths.setText( cnt.numSynths.toString )
		lbNumGroups.setText( cnt.numGroups.toString )
		lbNumDefs.setText( cnt.numDefs.toString )
	}
	
	private def clearCounts {
		lbCPU.update( 0, 0 )
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
//		private var avgCPU  = 0f
//		private var peakCPU = 0f
//		private var avgW    = 0
//		private var peakX   = 0
		private var peakCPU = 0 // 0...17

        private val imgGaugeEmpty = Toolkit.getDefaultToolkit.createImage( getClass.getResource( "gauge_empty.png" ))
        private val imgGaugeFull  = Toolkit.getDefaultToolkit.createImage( getClass.getResource( "gauge_full.png" ))

        // ---- constructor ----
        {
          addAncestorListener( new AncestorListener {
            def ancestorAdded( e: AncestorEvent ) {}
            def ancestorMoved( e: AncestorEvent ) {}
            def ancestorRemoved( e: AncestorEvent ) {
               imgGaugeEmpty.flush
               imgGaugeFull.flush
            }
          })
//          setPreferredSize( new Dimension( 73, 23 ))
          setPreferredSize( new Dimension( 54, 20 ))
        }
		
		def update( newAvgCPU: Float, newPeakCPU: Float ) {

//            val newPeakPix = max( 0, min( 73, (newPeakCPU * 73 + 0.5f).toInt ))
//            val newPeakPix = max( 0, min( 73, ((newPeakCPU * 18 + 0.5f).toInt * 4.06f).toInt ))
            val newPeakPix = max( 0, min( 54, (newPeakCPU * 18 + 0.5f).toInt * 3 ))

            if( newPeakPix != peakCPU ) {
              peakCPU = newPeakPix
              repaint() // could use dirty rec
            }

//			avgCPU  = newAvgCPU
//			peakCPU = newPeakCPU
//			val oldAvgW = avgW
//			val oldPeakX = peakX
//			updateScreenCoords
//			if( (oldAvgW != avgW) || (oldPeakX != peakX) ) repaint() // could use dirty rect
		}
		
		override def paintComponent( g: Graphics ) {
//			g.setColor( Color.black )
//			val w = getWidth
//			val h = getHeight
//			g.fillRect(  0, 0, w, h )
//			updateScreenCoords
//			g.setColor( Color.yellow /* Color.blue */)
//			g.fillRect( 1, 1, avgW, h - 2 )
//			g.drawLine( peakX, 1, peakX, h - 2 )
            
            g.drawImage( imgGaugeFull, 0, 0, peakCPU, 23, 0, 0, peakCPU, 23,
                         Color.black, this )
            g.drawImage( imgGaugeEmpty, peakCPU, 0, 73, 23, peakCPU, 0, 73, 23,
                         Color.black, this )
		}
		
//		private def updateScreenCoords {
//			val w = getWidth
//			avgW  = (avgCPU  * (w - 2)).toInt // + 1
//			peakX = (peakCPU * (w - 2)).toInt + 1
//		}
	}
}
