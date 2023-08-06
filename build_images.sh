#!/bin/bash

OPENING_TABLE_EXE_PATH=`stack exec -- which opening-table-exe`
# CHESS_EXE_PATH=`stack exec -- which chess-exe`
# docker-compose build --build-arg OPENING_TABLE_BINARY_PATH=$(realpath --relative-to="$PWD" "$OPENING_TABLE_EXE_PATH") --build-arg CHESS_BINARY_PATH=$(realpath --relative-to="$PWD" "$CHESS_EXE_PATH")
docker compose build --build-arg OPENING_TABLE_BINARY_PATH=$(realpath --relative-to="$PWD" "$OPENING_TABLE_EXE_PATH") --build-arg CACHE_DATE=$(date +%Y-%m-%d:%H:%M:%S)
# /home/ben/haskell/chess/.stack-work/install/x86_64-linux-tinfo6/4e5152c45ed7041a5af60378531401fbee6cf7df4bbfa58e94e68aa4540788da/9.2.7/bin
