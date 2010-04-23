---- ScalaCollider ----
A SuperCollider client for the Scala language.
Copyright (c) 2008-2010 Hanns Holger Rutz. All rights reserved.
Licensed under the GNU General Public License v2 (see "licenses" folder).

PREREQUISITES

Scala 2.8 (RC1) / Java 1.6 / SuperCollider 3.3.1+ / ScalaOSC

INSTALLATION

The following libraries have to be downloaded and installed into the "libraries" folder:

    - ScalaOSC (grab it from http://github.com/Sciss/ScalaOSC )

Compile using the included compile_fsc.sh script or the included IntelliJ IDEA 9 CE project.
    
DEMO SETUP

"Demo.command" should be a double-clickable shell script (on Mac; on Linux you should be able to run it with bash). It will launch scala (assuming that $SCALA_HOME was set) and run "DemoScript.txt". Edit "DemoScript.txt" to point to the right installation place of SuperCollider.

Try launching scsynth from the Scala interpreter prompt:

scala> s.boot

And when booted, create some example synths using "ExampleCmd.txt" as a template.