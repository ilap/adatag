#!/bin/bash

SRCDIR="./src"
TGTDIR="../adatag.io/src"

DIRS="workers configs services test"

for i in $DIRS
do
	rsync -avzP --delete "$SRCDIR/${i}/" "$TGTDIR/${i}/"
done

rsync -avzP src/index.ts ../adatag.io/src 
rsync -avzP src/util.ts ../adatag.io/src 
