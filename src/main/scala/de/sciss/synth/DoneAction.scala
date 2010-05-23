package de.sciss.synth

sealed abstract class DoneAction( val id: Int )

case object doNothing         extends DoneAction( 0 )
case object pauseSelf         extends DoneAction( 1 )
case object freeSelf          extends DoneAction( 2 )
case object freeSelfPred      extends DoneAction( 3 )
case object freeSelfSucc      extends DoneAction( 4 )
case object freeSelfPredAll   extends DoneAction( 5 )
case object freeSelfSuccAll   extends DoneAction( 6 )
case object freeSelfToHead    extends DoneAction( 7 )
case object freeSelfToTail    extends DoneAction( 8 )
case object freeSelfPausePred extends DoneAction( 9 )
case object freeSelfPauseSucc extends DoneAction( 10 )
case object freeSelfPredDeep  extends DoneAction( 11 )
case object freeSelfSuccDeep  extends DoneAction( 12 )
case object freeAllInGroup    extends DoneAction( 13 )
case object freeGroup         extends DoneAction( 14 )
