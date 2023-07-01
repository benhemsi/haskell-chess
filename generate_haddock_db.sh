#!/bin/bash

stack hoogle -- generate --local
STACK_HOOGLE_DB=`find .stack-work/hoogle/ -name *.hoo`
HOOGLE_DB=`find ~/.hoogle -name *.hoo`
cp $STACK_HOOGLE_DB $HOOGLE_DB
