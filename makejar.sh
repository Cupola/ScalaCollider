#!/bin/sh
echo "Archiving..."
mkdir dist
jar cf dist/ScalaCollider.jar -C build/classes/ .
jar uf dist/ScalaCollider.jar -C resources/ .
echo "Done."