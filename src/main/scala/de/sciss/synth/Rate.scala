package de.sciss.synth

/**
 *    @version 0.11, 23-May-10
 */
object Rate {
   def highest( rates: Rate* ) = rates.foldLeft[ Rate ]( scalar )( (a, b) => if( a.id > b.id ) a else b )
}

/**
 *    The calculation rate of a UGen or a UGen output.
 */
sealed abstract class Rate( val id: Int ) {
   val methodName: String
}

case object scalar  extends Rate( 0 ) { val methodName = "ir" }
case object control extends Rate( 1 ) { val methodName = "kr" }
case object audio   extends Rate( 2 ) { val methodName = "ar" }
case object demand  extends Rate( 3 ) { val methodName = "dr" }

trait RatedGE extends GE {
  def rate : Rate
}

trait ScalarRated  { def rate = scalar }
trait ControlRated { def rate = control }
trait AudioRated   { def rate = audio }
