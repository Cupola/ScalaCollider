/*
 *  EnvGen.scala
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
package de.sciss.tint.sc

import Rates._

object EnvGen {
  def ar( envelope: Env ) : GE = {
    ar( envelope, Constants.one, Constants.one, Constants.zero, Constants.one, Constants.zero )
   }
  
  def ar( envelope: Env, gate: GE ) : GE = {
    ar( envelope, gate, Constants.one, Constants.zero, Constants.one, Constants.zero )
   }
  
  def ar( envelope: Env, gate: GE, levelScale: GE ) : GE = {
    ar( envelope, gate, levelScale, Constants.zero, Constants.one, Constants.zero )
   }
  
  def ar( envelope: Env, gate: GE, levelScale: GE, levelBias: GE ) : GE = {
    ar( envelope, gate, levelScale, levelBias, Constants.one, Constants.zero )
   }
  
  def ar( envelope: Env, gate: GE, levelScale: GE, levelBias: GE, timeScale: GE ) : GE = {
    ar( envelope, gate, levelScale, levelBias, timeScale, Constants.zero )
   }
  
  def ar( envelope: Env, gate: GE, levelScale: GE, levelBias: GE, timeScale: GE, doneAction: GE ) : GE = {
  	ar( envelope.toArray, gate, levelScale, levelBias, timeScale, doneAction )
  }
  
  def ar( envArray: Array[ GE ], gate: GE, levelScale: GE, levelBias: GE, timeScale: GE, doneAction: GE ) : GE = {
  	UGen.multiNew( "EnvGen", audio, List( audio ), List( gate, levelScale, levelBias, timeScale, doneAction ) ++ envArray )
  }
  
  def kr( envelope: Env ) : GE = {
    kr( envelope, Constants.one, Constants.one, Constants.zero, Constants.one, Constants.zero )
   }
  
  def kr( envelope: Env, gate: GE ) : GE = {
    kr( envelope, gate, Constants.one, Constants.zero, Constants.one, Constants.zero )
   }
  
  def kr( envelope: Env, gate: GE, levelScale: GE ) : GE = {
    kr( envelope, gate, levelScale, Constants.zero, Constants.one, Constants.zero )
   }
  
  def kr( envelope: Env, gate: GE, levelScale: GE, levelBias: GE ) : GE = {
    kr( envelope, gate, levelScale, levelBias, Constants.one, Constants.zero )
   }
  
  def kr( envelope: Env, gate: GE, levelScale: GE, levelBias: GE, timeScale: GE ) : GE = {
    kr( envelope, gate, levelScale, levelBias, timeScale, Constants.zero )
   }
  
  def kr( envelope: Env, gate: GE, levelScale: GE, levelBias: GE, timeScale: GE, doneAction: GE ) : GE = {
  	kr( envelope.toArray, gate, levelScale, levelBias, timeScale, doneAction )
  }
  
  def kr( envArray: Array[ GE ], gate: GE, levelScale: GE, levelBias: GE, timeScale: GE, doneAction: GE ) : GE = {
  	UGen.multiNew( "EnvGen", control, List( control ), List( gate, levelScale, levelBias, timeScale, doneAction ) ++ envArray )
  }
}
