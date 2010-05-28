package de.sciss.synth

trait ClientOptionsLike {
   def clientID:              Int
   def nodeIDOffset:          Int
}

abstract class ClientOptions extends ClientOptionsLike

class ClientOptionsBuilder extends ClientOptionsLike {
   var clientID:              Int      = 0
   var nodeIDOffset:          Int      = 1000

   def build : ClientOptions = new Impl( clientID, nodeIDOffset )

   private class Impl( val clientID: Int, val nodeIDOffset: Int ) extends ClientOptions
}