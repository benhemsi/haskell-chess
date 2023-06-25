#!/bin/bash

STACK_EXE_PATH=`stack exec -- which chess-exe`
docker-compose build --build-arg BINARY_PATH=$(realpath --relative-to="$PWD" "$STACK_EXE_PATH")
docker-compose up -d
