#!/bin/sh
echo "Compiling..."
fsc -deprecation -cp libraries/ScalaOSC.jar -d out/production/ScalaCollider/ -sourcepath src/ src/de/sciss/tint/sc/*.scala src/de/sciss/tint/sc/ugen/*.scala src/de/sciss/tint/sc/swing/*.scala
sh makejar.sh
