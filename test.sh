#!/bin/bash

cat <<EOF | gcc -xc -c -o tmp2.o -
int ret3() { return 3; }
int ret5() { return 5; }
int add(int x, int y) { return x+y; }
int sub(int x, int y) { return x-y; }
int add6(int a, int b, int c, int d, int e, int f) {
  return a+b+c+d+e+f;
}
EOF

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
