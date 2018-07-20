#!/usr/bin/env bash

case $1 in
  build)
    stack build \
      --test --no-run-tests --bench --no-run-benchmarks
    ;;

  test)
    stack test
    ;;

  bench)
    stack bench
    ;;

  repl)
    stack repl
    ;;
esac
