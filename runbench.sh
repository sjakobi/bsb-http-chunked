#!/usr/bin/env bash

/opt/ghc/bin/cabal new-build --enable-benchmarks -O2

/home/simon/src/bsb-http-chunked/dist-newstyle/build/x86_64-linux/ghc-8.4.1/bsb-http-chunked-0.0.0.2/b/bench/opt/build/bench/bench --measure-with /home/simon/src/bsb-http-chunked/dist-newstyle/build/x86_64-linux/ghc-8.4.1/bsb-http-chunked-0.0.0.2/b/bench/opt/build/bench/bench --small
