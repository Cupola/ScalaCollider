package de.sciss.tint.sc

import SC._
// import _root_.scala.Predef._
import ugen._

object Examples {
	def main( args: Array[ String ]) {
//		LiveCoding.startInterpreting
//		example1
		debug
	}

	def debug {
		import SC._
		val x = play {
			val f = LFSaw.kr( 0.4 ).madd( 24, LFSaw.kr( 8 ).madd( 3, 80 )).midicps
     		CombN.ar(SinOsc.ar(f, 0) * 0.04, 0.2, 0.2, 4) // echoing sine wave
		}
	}
	
	def example1 {
		val options = new ServerOptions
		options.port.value = 57110
		val s = new Server( "local", options, 1 )
		
//		val ssp = new gui.ServerStatusPanel( s )
//		val frame1 = ssp.makeWindow
//		frame1.setVisible( true )
		
//		val ntp = new gui.NodeTreePanel( s )
//		val frame2 = ntp.makeWindow
//		frame2.setVisible( true )
		
		s.start
//		s.register()
		s.c.dumpOutgoingOSC( 1, System.out )
		s.c.dumpIncomingOSC( 1, System.out )
		s.startAliveThread()
		s.addListener( msg => msg match {
			case Server.Running => {
				println( "RUNNING!" )
				s.register()
				s.initTree
				val synth = babblingBrook
				s.nodeMgr.register( synth ) // XXX should be automatic
				println( synth )
			}
		})
		Thread.sleep( 10000 )
	}
	
	/*
	 *	A babbling brook example, by James McCartney 2007. See
	 *	http://supercollider.sourceforge.net/audiocode-examples/
	 */
	def babblingBrook : Synth = {
		(SynthDef( "test" ) {
//            val st: GE = List( 1, 1 )
            Out.ar( 0, (RHPF.ar( OnePole.ar( BrownNoise.ar( List( 1, 1 )), 0.99 ),
                              LPF.ar( BrownNoise.ar( List( 1, 1 )), 14 )
                        * 400 + 500, 0.03 ) * 0.003) +
                    (RHPF.ar( OnePole.ar( BrownNoise.ar( List( 1, 1 )), 0.99),
                              LPF.ar( BrownNoise.ar( List( 1, 1 )), 20 )
                        * 800 + 1000, 0.03 ) * 0.005) )
		}).play( Server.default )
	}
}
