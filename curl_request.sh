#!/bin/bash

curl --fail -X POST http://localhost:3000/evaluate/fens -H 'Content-Type: text/plain; charset=utf-8' -d \
"rnbqkbnr/pppppppp/8/8/8/8/1PPPPPPP/RNBQKBNR w KQkq - 0 1
rnbqkbnr/1ppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
