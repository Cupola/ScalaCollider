package de.sciss.synth

import de.sciss.scalaosc.OSCMessage

case class Completion[ T ]( message: Option[ T => OSCMessage ], action: Option[ T => Unit ])
