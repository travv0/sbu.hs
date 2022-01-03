#!/usr/bin/env sh

cabal install --installdir test/cram --overwrite-policy always && cram test && cabal test
