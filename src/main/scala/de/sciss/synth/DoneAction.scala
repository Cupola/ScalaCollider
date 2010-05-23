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
