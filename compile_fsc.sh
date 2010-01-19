#!/bin/sh
echo "Compiling..."
fsc -deprecation -cp libraries/ScalaOSC.jar -d build/classes/ -sourcepath src/ src/de/sciss/tint/sc/*.scala src/de/sciss/tint/sc/ugen/*.scala src/de/sciss/tint/sc/gui/*.scala
echo "Archiving..."
mkdir dist
jar cf dist/ScalaCollider.jar -C build/classes/ .
jar uf dist/ScalaCollider.jar -C resources/ .
echo "Done."