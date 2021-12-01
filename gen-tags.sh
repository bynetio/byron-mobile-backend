#!/bin/bash
rm -rf .deps
cabal get aeson text containers bytestring base servant servant-server warp wai co-log wai-cors optparse-applicative -d .deps
hasktags ./dist-newstyle/src .deps
