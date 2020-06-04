#!/bin/bash

assert() {
  want="$1"
  input="$2"

  cargo run -q -- "$input" > tmp.s
  cc -o tmp tmp.s
  ./tmp
  got="$?"

  if [ "$got" = "$want" ]; then
    echo "$input => $got"
  else
    echo "$input => $want want, but got $got"
    exit 1
  fi
}

assert 10 '-10+20'
assert 10 '- -10'
assert 10 '- - +10'

assert 41 ' 12 + 34 - 5 '

assert 21 '5+20-4'

assert 0 0
assert 42 42

echo OK