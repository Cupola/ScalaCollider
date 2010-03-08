/*
 *  NodeTreePanel.scala
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

import java.awt.{ BorderLayout, Color, Dimension }
import java.awt.event.{ ComponentAdapter, ComponentEvent }
import javax.swing.{ JFrame, JPanel, WindowConstants }

import de.sciss.tint.sc.{ Server }

/*
import _root_.edu.uci.ics.jung.algorithms.layout.{ BalloonLayout, DAGLayout, FRLayout,
	ISOMLayout, StaticLayout, TreeLayout }
import _root_.edu.uci.ics.jung.algorithms.layout.util.VisRunner
import _root_.edu.uci.ics.jung.graph.{ DelegateTree, DirectedGraph }
import _root_.edu.uci.ics.jung.graph.event.{ GraphEvent, GraphEventListener }
import _root_.edu.uci.ics.jung.graph.util.TreeUtils
import _root_.edu.uci.ics.jung.visualization.VisualizationViewer
import _root_.edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse
import _root_.edu.uci.ics.jung.visualization.decorators.ToStringLabeller
import _root_.edu.uci.ics.jung.visualization.layout.LayoutTransition
import _root_.edu.uci.ics.jung.visualization.renderers.Renderer
import _root_.edu.uci.ics.jung.visualization.util.Animator
*/
import _root_.scala.math._

/**
 *	@author		Hanns Holger Rutz
 *	@version	0.10, 27-Nov-09
 */
class NodeTreePanel( server: Server )
extends JPanel
/* with GraphEventListener[ Node, Long ] */
{
/*
	private val smartLayout = new FRLayout( server.nodeMgr.ograph )
//	private val forest = new DelegateTree( server.nodeMgr.graph )
//	println( "forest roots = " + TreeUtils.getRoots( server.nodeMgr.graph ))
//	private val testLayout = new TreeLayout( server.nodeMgr.graph )
//	forest.addVertex( new de.sciss.tint.sc.Group( server, 1 ))
//	private val smartLayout = new BalloonLayout( server.nodeMgr.graph ) // XXX ugly cast
//	private val smartLayout = new ISOMLayout( graph )
//	private val smartLayout = new DAGLayout( graph )
	private var vv: VisualizationViewer[ Node, Long ] = _
	
	// ---- constructor ----
	{
        smartLayout.setSize( new Dimension( 400, 400 ))
//		val staticLayout = new StaticLayout( server.nodeMgr.graph, smartLayout )
		
//      vv = new VisualizationViewer( staticLayout, new Dimension( 480, 440 )) 
        vv = new VisualizationViewer( smartLayout, new Dimension( 480, 440 )) 
        vv.setGraphMouse( new DefaultModalGraphMouse() )
        vv.getRenderer().getVertexLabelRenderer().setPosition( Renderer.VertexLabel.Position.CNTR )
        vv.getRenderContext().setVertexLabelTransformer( new ToStringLabeller() )
        vv.setBackground( Color.gray )
        vv.setForeground( Color.white )
      	addComponentListener( new ComponentAdapter() {
			override def componentResized( e: ComponentEvent ) {
				val dim = getSize()
				dim.width  = min( 32, dim.width  - 80 )
				dim.height = min( 32, dim.height - 40 )
				smartLayout.setSize( dim )
			}
        })
       	add( vv, BorderLayout.CENTER )
		
		addAncestorListener( new AncestorListener {
			def ancestorAdded( e: AncestorEvent ) {
				addListener
				updateLayout	
			}
			
			def ancestorRemoved( e: AncestorEvent ) {
				removeListener
			}
			
			def ancestorMoved( e: AncestorEvent ) {}
		})
	}
	
	private def updateLayout {
//		smartLayout.initialize()

//		val relaxer = new VisRunner( smartLayout )
//		relaxer.stop();
//		relaxer.prerelax();
//		val staticLayout = new StaticLayout( server.nodeMgr.graph, smartLayout )
//		val lt = new LayoutTransition( vv, vv.getGraphLayout, staticLayout )
//		val animator = new Animator( lt )
//		animator.start()
		vv.repaint()
	}

	private var addedListener = false
	
	private def addListener {
		if( !addedListener ) {
			addedListener = true
			server.nodeMgr.ograph.addGraphEventListener( this )
		}
	}
	
	private def removeListener {
		if( !addedListener ) {
			server.nodeMgr.ograph.removeGraphEventListener( this )
			addedListener = false
		}
	}

	// ---- GraphListener interface ----
	def handleGraphEvent( e: GraphEvent[ Node, Long ]) {
		updateLayout
	}
*/
	def makeWindow: JFrame = {
		val frame = new JFrame( "Nodes" )
//		frame.setResizable( false )
		frame.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
		frame.getContentPane.add( this )
		frame.pack()
		frame.setVisible( true )
		frame
	}
}
