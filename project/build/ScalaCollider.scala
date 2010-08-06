import sbt._
import java.io.{ IOException, RandomAccessFile }

class ScalaColliderProject( info: ProjectInfo ) extends DefaultProject( info ) { 
   val dep1 = "de.sciss" %% "scalaosc" % "0.18"
   val dep2 = "de.sciss" %% "scalaaudiofile" % "0.13"
   lazy val demo = demoAction

   protected def demoAction = {
       val consoleInit = try {
           val raf = new RandomAccessFile( "console-startup.txt", "r" )
           val buf = new Array[ Byte ]( raf.length.toInt )
           raf.readFully( buf )
           raf.close()
           new String( buf, "UTF-8" )
       }
       catch { case e: IOException => { e.printStackTrace(); "" }}
       consoleTask( consoleClasspath, consoleInit ) describedAs "Starts a ScalaCollider REPL, initializing from file \"console-startup.txt\"."
   }
}
