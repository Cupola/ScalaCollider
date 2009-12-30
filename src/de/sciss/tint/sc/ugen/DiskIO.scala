package de.sciss.tint.sc.ugen

import de.sciss.tint.sc._
import SC._
//import Rates._

/**
 * 	@version	0.10, 19-Jul-09
 */
object DiskOut {	
	def ar( bufNum: GE, channelsArray: GE ) : GE = {
    	UGen.multiNew( "DiskOut", audio, Nil, bufNum :: channelsArray.toUGenInputs.toList )
	}
}

object DiskIn {	
	def ar( numChannels: Int, bufNum: GE, loop: GE = 0 ) : GE = {
    	UGen.multiNew( "DiskIn", audio, dup( audio, numChannels ), List( bufNum, loop ))
	}

	def ar( buf: Buffer ) : GE = {
		ar( buf, 0 )
	}
	
	def ar( buf: Buffer, loop: GE ) : GE = {
		UGen.multiNew( "DiskIn", audio, dup( audio, buf.numChannels ), List( buf.bufNum, loop ))
	}
}

object VDiskIn {	
	def ar( numChannels: Int, bufNum: GE, rate: GE = 1, loop: GE = 0, sendID: GE = 0 ) : GE = {
    	UGen.multiNew( "VDiskIn", audio, dup( audio, numChannels ), List( bufNum, rate, loop, sendID ))
	}

	// unfortunately we cannot define two ar methods with default args...
	def ar( buf: Buffer ) : GE = {
		ar( buf, 1, 0, 0 )
	}
	
	def ar( buf: Buffer, rate: GE ) : GE = {
		ar( buf, rate, 0, 0 )
	}
	
	def ar( buf: Buffer, rate: GE, loop: GE ) : GE = {
		ar( buf, rate, loop, 0 )
	}
	
	def ar( buf: Buffer, rate: GE, loop: GE, sendID: GE ) : GE = {
		UGen.multiNew( "VDiskIn", audio, dup( audio, buf.numChannels ), List( buf.bufNum, rate, loop, sendID ))
	}
}