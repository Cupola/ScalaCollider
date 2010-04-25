#!/bin/sh
echo "Archiving..."
mkdir dist
jar cf dist/ScalaCollider.jar -C out/production/ScalaCollider/ .
# jar uf dist/ScalaCollider.jar -C resources/ .
echo "Done."
