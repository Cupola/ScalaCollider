COPYRIGHT

Please check out the "licenses" folder for license information.

PREREQUISITES

Java 1.6 (might work in 1.5) / Scala 2.8 (BETA1) / SuperCollider 3.3.1 / ScalaOSC

INSTALLATION

Note: to keep the repo small, the following libraries have not been included and need to be installed manually into the "libraries" folder:

    - ScalaOSC (grab it from http://github.com/Sciss/ScalaOSC )

Compile using the included compile_fsc.sh script or the included IntellJ IDEA project. Note that the repo also contains the previous NetBeans project. Due to problems with the NetBeans plug-in, the project now uses IDEA, the nb project files are still there for reference.
    
DEMO SETUP

"Demo.command" should be a double-clickable shell script (on Mac; on Linux you should be able to run it with bash). It will launch scala (assuming that $SCALA_HOME was set and the scala binaries are in $PATH) and run "DemoScript.txt". Edit "DemoScript.txt" to point to the right installation place of SuperCollider.

Try launching scsynth from the Scala interpreter prompt:

scala> s.boot

And when booted, create some example synths using stuff in "ExampleCmd.txt" as a template.