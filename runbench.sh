#!/usr/bin/env bash

/opt/ghc/bin/cabal new-build --enable-benchmarks -O2 -w ghc-8.4.3

/home/simon/src/bsb-http-chunked/dist-newstyle/build/x86_64-linux/ghc-8.4.3/bsb-http-chunked-0.0.0.3/b/bench/opt/build/bench/bench --measure-with /home/simon/src/bsb-http-chunked/dist-newstyle/build/x86_64-linux/ghc-8.4.3/bsb-http-chunked-0.0.0.3/b/bench/opt/build/bench/bench --small
