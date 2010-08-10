package de.sciss.synth

import de.sciss.osc.OSCMessage

case class Completion[ T ]( message: Option[ T => OSCMessage ], action: Option[ T => Unit ])
