#!/bin/sh
echo "Compiling..."
fsc -deprecation -cp libraries/ScalaOSC.jar -d out/production/ScalaCollider/ -sourcepath src/ src/de/sciss/synth/*.scala src/de/sciss/synth/ugen/*.scala
sh makejar.sh
