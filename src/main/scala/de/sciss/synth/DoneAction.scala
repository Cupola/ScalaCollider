/*
 *  DoneAction.scala
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

sealed abstract class DoneAction( val id: Int )

/**
 *    Does nothing in particular when the UGen is finished
 */
case object doNothing         extends DoneAction( 0 )
/**
 *    Pauses the enclosing synth when the UGen is finished
 */
case object pauseSelf         extends DoneAction( 1 )
/**
 *    Frees the enclosing synth when the UGen is finished
 */
case object freeSelf          extends DoneAction( 2 )
/**
 *    Frees the enclosing synth as well as the predecessor node
 *    when the UGen is finished
 */
case object freeSelfPred      extends DoneAction( 3 )
/**
 *    Frees the enclosing synth as well as the successor node
 *    when the UGen is finished
 */
case object freeSelfSucc      extends DoneAction( 4 )
/**
 *    Frees the enclosing synth when the UGen is finished.
 *    If the predecessor node is a group, calls freeAll on
 *    that group. If the predecssor node is a synth,
 *    frees that synth.
 */
case object freeSelfPredAll   extends DoneAction( 5 )
/**
 *    Frees the enclosing synth when the UGen is finished.
 *    If the successor node is a group, calls freeAll on
 *    that group. If the successor node is a synth,
 *    frees that synth.
 */
case object freeSelfSuccAll   extends DoneAction( 6 )
/**
 *    Frees the enclosing synth and all preceding nodes
 *    in its group when the UGen is finished
 */
case object freeSelfToHead    extends DoneAction( 7 )
/**
 *    Frees the enclosing synth and all succeeding nodes
 *    in its group when the UGen is finished
 */
case object freeSelfToTail    extends DoneAction( 8 )
/**
 *    Frees the enclosing synth and pauses the predecessor node
 *    when the UGen is finished
 */
case object freeSelfPausePred extends DoneAction( 9 )
/**
 *    Frees the enclosing synth and pauses the successor node
 *    when the UGen is finished
 */
case object freeSelfPauseSucc extends DoneAction( 10 )
/**
 *    Frees the enclosing synth when the UGen is finished.
 *    If the predecessor node is a group, calls deepFree on
 *    that group. If the predecessor node is a synth,
 *    frees that synth.
 */
case object freeSelfPredDeep  extends DoneAction( 11 )
/**
 *    Frees the enclosing synth when the UGen is finished.
 *    If the successor node is a group, calls deepFree on
 *    that group. If the successor node is a synth,
 *    frees that synth.
 */
case object freeSelfSuccDeep  extends DoneAction( 12 )
/**
 *    Frees the enclosing synth along with all other nodes
 *    in the group when the UGen is finished (i.e. does
 *    a freeAll on the group)
 */
case object freeAllInGroup    extends DoneAction( 13 )
/**
 *    Frees the enclosing group when the UGen is finished,
 *    and hence also frees this synth along with all other
 *    nodes in the group.
 */
case object freeGroup         extends DoneAction( 14 )
