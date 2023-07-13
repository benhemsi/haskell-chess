#!/bin/bash

OPENING_TABLE_EXE_PATH=`stack exec -- which opening-table-exe`
# CHESS_EXE_PATH=`stack exec -- which chess-exe`
# docker-compose build --build-arg OPENING_TABLE_BINARY_PATH=$(realpath --relative-to="$PWD" "$OPENING_TABLE_EXE_PATH") --build-arg CHESS_BINARY_PATH=$(realpath --relative-to="$PWD" "$CHESS_EXE_PATH")
docker-compose build --build-arg OPENING_TABLE_BINARY_PATH=$(realpath --relative-to="$PWD" "$OPENING_TABLE_EXE_PATH")
