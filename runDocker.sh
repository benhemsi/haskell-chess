#!/bin/bash

STACK_EXE_PATH=$(realpath --relative-to="${PWD}" `stack exec -- which chess-exe`)
docker-compose run -e BINARY_PATH=${STACK_EXE_PATH} chess
