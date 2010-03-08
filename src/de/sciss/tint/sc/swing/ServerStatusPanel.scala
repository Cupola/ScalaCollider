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
package de.sciss.tint.sc.swing

import java.awt.{ Color, Component, Container, Dimension, FlowLayout, Font,
                 Graphics, Image, Toolkit }
import java.awt.event.{ ActionEvent }
import javax.swing.{ AbstractAction, BorderFactory, Box, BoxLayout, ImageIcon, JButton,
                    JComponent, JFrame, JLabel, JPanel, JProgressBar,
                    OverlayLayout, SwingConstants, WindowConstants }
import SwingConstants._
import javax.swing.event.{ AncestorEvent, AncestorListener }
import scala.math._

import de.sciss.tint.sc.{ OSCStatusReplyMessage, Server }

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.11, 12-Jan-10
 */
object ServerStatusPanel {
  val COUNTS      = 0x01
  val BOOT_BUTTON = 0x02
}

class ServerStatusPanel( flags: Int ) extends JPanel {
  import ServerStatusPanel._
  
    def this( s: Server, flags: Int ) {
      this( flags )
      server = Some( s )
    }

    def this( s: Server ) {
      this( s, 0x03 ) // XXX weird scala bug... does not see COUNTS and BOOT_BUTTON
    }

   private val actionBoot   = new ActionBoot()
   private val ggBoot       = new JButton( actionBoot )
   private val ggBusy       = new JProgressBar()

   // subclasses may override this
   protected def txtBoot    = "Boot"  // XXX getResource
   protected def txtStop    = "Stop"  // XXX getResource
   protected def frameTitle = "Server Status"

	private val lbCPU		= new CPUIndicator
	private val lbNumUGens	= new CountLabel
	private val lbNumSynths	= new CountLabel
	private val lbNumGroups	= new CountLabel
	private val lbNumDefs	= new CountLabel

    private var serverVar: Option[ Server ] = None
    def server = serverVar
    def server_=( s: Option[ Server ]) {
       val wasListening = listening
       if( wasListening ) stopListening
       serverVar = s
       updateFrameTitle
       if( wasListening ) startListening
    }
		
	// ---- constructor ----
	{
		setLayout( new BoxLayout( this, BoxLayout.X_AXIS ))

        val clz = classOf[ ServerStatusPanel ]
        val icnGroup = new ImageIcon( clz.getResource( "path_group_16.png" ))
        val icnSynth = new ImageIcon( clz.getResource( "path_synth_16.png" ))
        val icnUGen  = new ImageIcon( clz.getResource( "path_ugen_16.png" ))
        val icnDef   = new ImageIcon( clz.getResource( "path_def_16.png" ))

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

        if( (flags & BOOT_BUTTON) != 0 ) {
    		ggBoot.setFocusable( false )	// prevent user from accidentally starting/stopping server
            ggBoot.putClientProperty( "JButton.buttonType", "bevel" )
            ggBoot.putClientProperty( "JComponent.sizeVariant", "small" )
            ggBoot.setText( txtStop )
            val d1 = ggBoot.getPreferredSize()
            ggBoot.setText( txtBoot )
            val d2 = ggBoot.getPreferredSize()
            ggBoot.setPreferredSize( new Dimension( max( d1.width, d2.width ),
                                                    max( d1.height, d2.height )))

            ggBusy.setIndeterminate( true )
            val busyDim = new Dimension( 24, 24 )
            ggBusy.setPreferredSize( busyDim )
            ggBusy.putClientProperty( "JProgressBar.style", "circular" )

            addS( ggBoot, 2 )
            val busyBox = new JPanel()
            busyBox.setLayout( new OverlayLayout( busyBox ))
            busyBox.add( Box.createRigidArea( busyDim ))
            busyBox.add( ggBusy )
            addS( busyBox, 6 )

    		setBorder( BorderFactory.createEmptyBorder( 0, 2, 0, 2 ))
        } else {
    		setBorder( BorderFactory.createEmptyBorder( 1, 2, 1, 2 ))
        }
        
        if( (flags & COUNTS) != 0 ) {
    		addS( lbCPU, 8 )
            def addCount( icn: ImageIcon, lb: JLabel, s: Int = 4 ) {
                val lb2 = new JLabel( icn )
                lb2.putClientProperty( "JComponent.sizeVariant", "small" )
            	addS( lb2 )
        		addS( lb, s )
            }
            addCount( icnGroup, lbNumGroups )
            addCount( icnSynth, lbNumSynths )
            addCount( icnUGen,  lbNumUGens )
            addCount( icnDef,   lbNumDefs, 0 )
        }

//        serverUpdate( server.map( _.condition ) getOrElse Server.Offline )

//        val fntName = if( System.getProperty( "os.name" ).indexOf( "Mac OS X" ) >= 0 )
//          "Lucida Grande" else "SansSerif"
//		val fntGUI = new Font( fntName, Font.PLAIN, 9 )
//		setDeepFont( this, fntGUI )
		
		addAncestorListener( new AncestorListener {
			def ancestorAdded( e: AncestorEvent ) {
				startListening
//				updateCounts
			}
			
			def ancestorRemoved( e: AncestorEvent ) {
				stopListening
                flushImages
//				updateCounts
			}
			
			def ancestorMoved( e: AncestorEvent ) {}
		})
	}

    protected def couldBoot: Boolean = server.isDefined

    private var frame: Option[ JFrame ] = None

    private def updateFrameTitle {
       frame.foreach( _.setTitle( if( server.isDefined ) {
         frameTitle + " (" + server.get.name + ")"
       } else {
         frameTitle
       }))
    }

	def makeWindow: JFrame = {
        val f = frame getOrElse {
    		val fr = new JFrame()
        	fr.setResizable( false )
            fr.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
      		fr.getContentPane.add( this )
        	fr.pack()
            fr.setLocation( 50, 50 )
            frame = Some( fr )
            updateFrameTitle
            fr
        }
		f.setVisible( true )
		f
	}
	
	private var listening = false
	
	private def startListening {
		if( !listening ) {
//println("startListening")
			listening = true
			serverVar.foreach(_.addListener( serverUpdate ))
            serverUpdate( server.map( _.condition ) getOrElse Server.Offline )
		}
	}
	
	private def stopListening {
		if( listening ) {
//println("stopListening")
			serverVar.foreach(_.removeListener( serverUpdate ))
			listening = false
            clearCounts
		}
	}
	
//	private var srv: Option[ Server ] = None
//	def server = srv
//	def server_=( newServer: Option[ Server ]) {
//		if( srv == newServer ) return
//		val wasAdded = listening
//		stopListening
//		srv = newServer
//		if( isShowing ) {
//			updateCounts
//			if( srv.isDefined ) startListening
//		}
//	}
	
	private def serverUpdate( msg: AnyRef ) : Unit = msg match {
      case Server.Counts( cnt ) if isShowing => updateCounts( cnt )
      case Server.Offline => {
          clearCounts
          actionBoot.serverUpdate( msg )
      }
      case _ => actionBoot.serverUpdate( msg )
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
	
//	private def setDeepFont( c: Component, fnt: Font ) {
//		c.setFont( fnt )
//		c match {
//			case con: Container => con.getComponents.foreach( setDeepFont( _, fnt ))
//			case _ =>
//		}
//	}

    private class CountLabel extends JLabel() {
        putClientProperty( "JComponent.sizeVariant", "small" )

        override def getPreferredSize() : Dimension = {
          val dim = super.getPreferredSize()
          dim.width = 40
          dim
        }

        override def getMinimumSize() : Dimension = getPreferredSize()
        override def getMaximumSize() : Dimension = getPreferredSize()
    }
	
	private class CPUIndicator extends JComponent {
//		private var avgCPU  = 0f
//		private var peakCPU = 0f
//		private var avgW    = 0
//		private var peakX   = 0
		private var peakCPU = 0 // 0...17

        private def getImageResource( name: String ) : Image =
          Toolkit.getDefaultToolkit.createImage( classOf[ CPUIndicator ].getResource( name ))

        private val imgGaugeEmpty = getImageResource( "gauge_empty.png" )
        private val imgGaugeFull  = getImageResource( "gauge_full.png" )

//        private val ins = getInsets()

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
          val dim = new Dimension( 56, 22 )
          setPreferredSize( dim )
          setMaximumSize( dim )
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

//        private val colrBorder = new Color( 0, 0, 0, 0xB4 )
//        private val colrEdge   = new Color( 0, 0, 0, 0x7F )
        private val colrBorder = new Color( 0, 0, 0, 0x35 )

		override def paintComponent( g: Graphics ) {
//			g.setColor( Color.black )
//			val w = getWidth
//			val h = getHeight
//			g.fillRect(  0, 0, w, h )
//			updateScreenCoords
//			g.setColor( Color.yellow /* Color.blue */)
//			g.fillRect( 1, 1, avgW, h - 2 )
//			g.drawLine( peakX, 1, peakX, h - 2 )
            g.setColor( colrBorder )
            g.drawRect( 0, 0, 55, 21 )
            g.drawRect( 1, 0, 53, 21 )
            g.drawRect( 0, 1, 55, 19 )
            g.drawImage( imgGaugeFull, 1, 1,
                         peakCPU + 1, 21, 0, 0, peakCPU, 20,
                         Color.black, this )
            g.drawImage( imgGaugeEmpty, peakCPU + 1, 1,
                         55, 21, peakCPU, 0, 54, 20,
                         Color.black, this )
		}
		
//		private def updateScreenCoords {
//			val w = getWidth
//			avgW  = (avgCPU  * (w - 2)).toInt // + 1
//			peakX = (peakCPU * (w - 2)).toInt + 1
//		}
	}

    // subclasses may override this
    protected def bootServer {
      server.foreach( _.boot )
    }

    // subclasses may override this
    protected def stopServer {
      server.foreach( _.quit )
    }

	private class ActionBoot extends AbstractAction {
       import Server._

       private var cond: AnyRef = Offline

		def actionPerformed( e: ActionEvent ) {
			if( cond == Offline ) {
				bootServer
			} else if( cond == Running ) {
				stopServer
			}
		}

      def serverUpdate( msg: AnyRef ) : Unit = msg match {
          case Server.Running => {
              cond = msg
              ggBoot.setText( txtStop )
              ggBoot.setEnabled( true )
              ggBusy.setVisible( false )
          }
          case Server.Offline => {
              cond = msg
              ggBoot.setText( txtBoot )
              ggBoot.setEnabled( couldBoot )
              ggBusy.setVisible( false )
          }
          case Server.Booting => {
              cond = msg
              ggBoot.setEnabled( false )
              ggBusy.setVisible( true )
          }
//          case SuperColliderClient.ServerChanged( server ) => {
//            serverPanel.server = server
//          }
      }
	} // class actionBootClass
}
