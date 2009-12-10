COPYRIGHT

Please check out the "licenses" folder for license information.

PREREQUISITES

Java 1.5 / Scala 2.8 / SuperCollider 3.3.1 / JUNG 2.0 / ScalaOSC (included).

INSTALLATION

Note: to keep the repo small, the following libraries have not been included and need to be installed manually into the "libraries" folder:

for JUNG 2.0:
	collections-generic-4.01.jar
	jung-algorithms-2.0.jar
	jung-api-2.0.jar
	jung-graph-impl-2.0.jar
	jung-visualization-2.0.jar

DEMO SETUP

"Demo.command" should be a double-clickable shell script (on Mac; on Linux you should be able to run it with bash). It will launch scala (assuming that $SCALA_HOME was set and the scala binaries are in $PATH) and run "DemoScript.txt". Edit "DemoScript.txt" to point to the right installation place of SuperCollider.

Try launching scsynth from the Scala interpreter prompt:

scala> s.boot

And when booted, create some example synths using stuff in "ExampleCmd.txt" as a template.