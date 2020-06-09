#!/bin/bash

assert() {
  build="$1"
  want="$2"
  input="$3"

  if [ "$build" = "debug" ]; then
    time cargo run -q -- "$input" > tmp.s || exit
    cc -o tmp tmp.s
    time ./tmp
  else
    cargo run -q --release -- "$input" > tmp.s || exit
    cc -o tmp tmp.s
    ./tmp
  fi

  got="$?"

  if [ "$got" = "$want" ]; then
    echo "$input => $got"
  else
    echo "$input => $want want, but got $got"
    exit 1
  fi
}

build="$1"

assert $build 10 '{ return -10+20; }'
assert $build 10 '{ return - -10; }'
assert $build 10 '{ return - - +10; }'

assert $build 41 '{ return  12 + 34 - 5 ; }'

assert $build 21 '{ return 5+20-4; }'

assert $build 0 '{ return 0; }'
assert $build 42 '{ return 42; }'

assert $build 0 '{ return 0==1; }'
assert $build 1 '{ return 42==42; }'
assert $build 1 '{ return 0!=1; }'
assert $build 0 '{ return 42!=42; }'

assert $build 1 '{ return 0<1; }'
assert $build 0 '{ return 1<1; }'
assert $build 0 '{ return 2<1; }'
assert $build 1 '{ return 0<=1; }'
assert $build 1 '{ return 1<=1; }'
assert $build 0 '{ return 2<=1; }'

assert $build 1 '{ return 1>0; }'
assert $build 0 '{ return 1>1; }'
assert $build 0 '{ return 1>2; }'
assert $build 1 '{ return 1>=0; }'
assert $build 1 '{ return 1>=1; }'
assert $build 0 '{ return 1>=2; }'

assert $build 1 '{ return 1; 2; 3; }'
assert $build 2 '{ 1; return 2; 3; }'
assert $build 3 '{ 1; 2; return 3; }'

assert $build 3 '{ a=3; return a; }'
assert $build 8 '{ a=3; z=5; return a+z; }'
assert $build 6 '{ a=b=3; return a+b; }'

assert $build 3 '{ foo=3; return foo; }'
assert $build 8 '{ foo123=3; bar=5; return foo123+bar; }'

assert $build 3 '{ if (0) return 2; return 3; }'
assert $build 3 '{ if (1-1) return 2; return 3; }'
assert $build 2 '{ if (1) return 2; return 3; }'
assert $build 2 '{ if (2-1) return 2; return 3; }'

assert $build 55 '{ i=0; j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }'
assert $build 3 '{ for (;;) return 3; return 5; }'
assert $build 10 '{ i=0; j=0; k=0; for (i=0; i<=10; i=i+1) for (k=0; k<3;k=k+1) k = 2; return 10; }'

assert $build 10 '{ i=0; while(i<10) i=i+1; return i; }'

assert $build 3 '{ {1; {2;} return 3;} }'

assert $build 0 '{ return -(+1-1); }'

assert $build 3 '{ x=3; return *&x; }'
assert $build 3 '{ x=3; y=&x; z=&y; return **z; }'
assert $build 5 '{ x=3; y=&x; *y=5; return x; }'

# The local variable order is chibicc's ABI.
assert $build 5 '{ x=3; y=5; return *(&x+1); }'
assert $build 3 '{ x=3; y=5; return *(&y-1); }'
assert $build 7 '{ x=3; y=5; *(&x+1)=7; return y; }'
assert $build 7 '{ x=3; y=5; *(&y-1)=7; return x; }'

assert $build 2 '{ x=3; return (&x+2)-&x; }'
echo OK