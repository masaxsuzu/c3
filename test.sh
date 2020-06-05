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

assert 10 'return -10+20;'
assert 10 'return - -10;'
assert 10 'return - - +10;'

assert 41 'return  12 + 34 - 5 ;'

assert 21 'return 5+20-4;'

assert 0 'return 0;'
assert 42 'return 42;'

assert 0 'return 0==1;'
assert 1 'return 42==42;'
assert 1 'return 0!=1;'
assert 0 'return 42!=42;'

assert 1 'return 0<1;'
assert 0 'return 1<1;'
assert 0 'return 2<1;'
assert 1 'return 0<=1;'
assert 1 'return 1<=1;'
assert 0 'return 2<=1;'

assert 1 'return 1>0;'
assert 0 'return 1>1;'
assert 0 'return 1>2;'
assert 1 'return 1>=0;'
assert 1 'return 1>=1;'
assert 0 'return 1>=2;'

assert 1 'return 1; 2; 3;'
assert 2 '1; return 2; 3;'
assert 3 '1; 2; return 3;'

assert 3 'a=3; return a;'
assert 8 'a=3; z=5; return a+z;'
assert 6 'a=b=3; return a+b;'

echo OK