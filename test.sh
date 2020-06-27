#!/bin/bash

assert() {
  build="$1"
  want="$2"
  input="$3"

  if [ "$build" = "debug" ]; then
    time cargo run -q -- "$input" > tmp.s || exit
    cc -static -o tmp tmp.s
    time ./tmp
  else
    cargo run -q --release -- "$input" > tmp.s || exit
    cc -static -o tmp tmp.s
    ./tmp
  fi

  got="$?"

  if [ "$got" = "$want" ]; then
    echo "$input => $got"
  else
    exit 1
  fi
}

build="$1"
input="$2"

assert $build 0 "$input"
