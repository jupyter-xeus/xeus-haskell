#!/bin/bash
docker run --rm -i -v "$(pwd):/work" xeus-haskell "ctest --test-dir /tmp --output-on-failure ."
