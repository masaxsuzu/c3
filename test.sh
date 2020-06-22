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
    cc -static -o tmp tmp.s tmp2.o
    time ./tmp
  else
    cargo run -q --release -- "$input" > tmp.s || exit
    cc -static -o tmp tmp.s tmp2.o
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

assert $build 10 'int main() { return -10+20; }'
assert $build 10 'int main() { return - -10; }'
assert $build 10 'int main() { return - - +10; }'

assert $build 41 'int main() { return  12 + 34 - 5 ; }'

assert $build 21 'int main() { return 5+20-4; }'

assert $build 0 'int main() { return 0; }'
assert $build 42 'int main() { return 42; }'

assert $build 0 'int main() { return 0==1; }'
assert $build 1 'int main() { return 42==42; }'
assert $build 1 'int main() { return 0!=1; }'
assert $build 0 'int main() { return 42!=42; }'

assert $build 1 'int main() { return 0<1; }'
assert $build 0 'int main() { return 1<1; }'
assert $build 0 'int main() { return 2<1; }'
assert $build 1 'int main() { return 0<=1; }'
assert $build 1 'int main() { return 1<=1; }'
assert $build 0 'int main() { return 2<=1; }'

assert $build 1 'int main() { return 1>0; }'
assert $build 0 'int main() { return 1>1; }'
assert $build 0 'int main() { return 1>2; }'
assert $build 1 'int main() { return 1>=0; }'
assert $build 1 'int main() { return 1>=1; }'
assert $build 0 'int main() { return 1>=2; }'

assert $build 1 'int main() { return 1; 2; 3; }'
assert $build 2 'int main() { 1; return 2; 3; }'
assert $build 3 'int main() { 1; 2; return 3; }'

assert $build 3 'int main() { int a=3; return a; }'
assert $build 8 'int main() { int a=3; int z=5; return a+z; }'
assert $build 6 'int main() { int a; int b; a=b=3; return a+b; }'

assert $build 3 'int main() { int foo=3; return foo; }'
assert $build 8 'int main() { int foo123=3; int bar=5; return foo123+bar; }'

assert $build 3 'int main() { if (0) return 2; return 3; }'
assert $build 3 'int main() { if (1-1) return 2; return 3; }'
assert $build 2 'int main() { if (1) return 2; return 3; }'
assert $build 2 'int main() { if (2-1) return 2; return 3; }'

assert $build 55 'int main() { int i=0; int j=0; for (i=0; i<=10; i=i+1) j=i+j; return j; }'
assert $build 3 'int main() { for (;;) return 3; return 5; }'
assert $build 10 'int main() { int i=0; int j=0; int k=0; for (i=0; i<=10; i=i+1) for (k=0; k<3;k=k+1) k = 2; return 10; }'

assert $build 10 'int main() { int i=0; while(i<10) i=i+1; return i; }'

assert $build 3 'int main() { {1; {2;} return 3;} }'

assert $build 0 'int main() { return -(+1-1); }'

assert $build 3 'int main() { int x=3; return *&x; }'
assert $build 3 'int main() { int x=3; int *y=&x; int **z=&y; return **z; }'
assert $build 5 'int main() { int x=3; int *y=&x; *y=5; return x; }'

# The local variable order is chibicc's ABI.
assert $build 5 'int main() { int x=3; int y=5; return *(&x+1); }'
assert $build 3 'int main() { int x=3; int y=5; return *(&y-1); }'
assert $build 7 'int main() { int x=3; int y=5; *(&x+1)=7; return y; }'
assert $build 7 'int main() { int x=3; int y=5; *(&y-1)=7; return x; }'

assert $build 2 'int main() { int x=3; return (&x+2)-&x; }'
assert $build 8 'int main() { int x=3, y=5; return x+y; }'
assert $build 8 'int main() { int x, y; x=3; y=5; return x+y; }'

assert $build 3 'int main() { return ret3(); }'
assert $build 5 'int main() { return ret5(); }'
assert $build 16 'int main() { int x = ret3()*ret5() + 1; return x; }'
assert $build 8 'int main() { return add(3, 5); }'
assert $build 2 'int main() { return sub(5, 3); }'
assert $build 21 'int main() { return add6(1,2,3,4,5,6); }'

assert $build 42 'int retx() { int x = 42; return x; } int main() { return retx();}'
assert $build 42 'int retx(int x) { return x; } int main() { return retx(42);}'
assert $build 3 'int s(int x1, int x2, int x3, int x4, int x5, int x6) { return x1 - x2 + x3 - x4 + x5 - x6; } int main() { return s(6,5,4,3,2,1);}'
assert $build 7 'int main() { return add2(3,4); } int add2(int x, int y) { return x+y; }'
assert $build 1 'int main() { return sub2(4,3); } int sub2(int x, int y) { return x-y; }'
assert $build 55 'int main() { return fib(9); } int fib(int x) { if (x<=1) return 1; return fib(x-1) + fib(x-2); }'


assert $build 3 'int main() { int x[2]; int *y=&x; *y=3; return *x; }'
assert $build 3 'int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *x; }'
assert $build 4 'int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *(x+1); }'
assert $build 5 'int main() { int x[3]; *x=3; *(x+1)=4; *(x+2)=5; return *(x+2); }'
assert $build 0 'int main() { int x[2][3]; int *y=x; *y=0; return **x; }'
assert $build 1 'int main() { int x[2][3]; int *y=x; *(y+1)=1; return *(*x+1); }'
assert $build 2 'int main() { int x[2][3]; int *y=x; *(y+2)=2; return *(*x+2); }'
assert $build 3 'int main() { int x[2][3]; int *y=x; *(y+3)=3; return **(x+1); }'
assert $build 4 'int main() { int x[2][3]; int *y=x; *(y+4)=4; return *(*(x+1)+1); }'
assert $build 5 'int main() { int x[2][3]; int *y=x; *(y+5)=5; return *(*(x+1)+2); }'

assert $build 5 'int main() { int x[2][3]; int *y=x+1-1; *(y)=5; return **x; }'

assert $build 3 'int main() { int x[3]; *x=3; x[1]=4; x[2]=5; return *x; }'
assert $build 4 'int main() { int x[3]; *x=3; x[1]=4; x[2]=5; return *(x+1); }'
assert $build 5 'int main() { int x[3]; *x=3; x[1]=4; x[2]=5; return *(x+2); }'
assert $build 5 'int main() { int x[3]; *x=3; x[1]=4; x[2]=5; return *(x+2); }'
assert $build 5 'int main() { int x[3]; *x=3; x[1]=4; 2[x]=5; return *(x+2); }'

assert $build 0 'int main() { int x[2][3]; int *y=x; y[0]=0; return x[0][0]; }'
assert $build 1 'int main() { int x[2][3]; int *y=x; y[1]=1; return x[0][1]; }'
assert $build 2 'int main() { int x[2][3]; int *y=x; y[2]=2; return x[0][2]; }'
assert $build 3 'int main() { int x[2][3]; int *y=x; y[3]=3; return x[1][0]; }'
assert $build 4 'int main() { int x[2][3]; int *y=x; y[4]=4; return x[1][1]; }'
assert $build 5 'int main() { int x[2][3]; int *y=x; y[5]=5; return x[1][2]; }'

assert $build 8 'int main() { int x; return sizeof(x); }'
assert $build 8 'int main() { int x; return sizeof x; }'
assert $build 8 'int main() { int *x; return sizeof(x); }'
assert $build 32 'int main() { int x[4]; return sizeof(x); }'
assert $build 96 'int main() { int x[3][4]; return sizeof(x); }'
assert $build 32 'int main() { int x[3][4]; return sizeof(*x); }'
assert $build 8 'int main() { int x[3][4]; return sizeof(**x); }'
assert $build 9 'int main() { int x[3][4]; return sizeof(**x) + 1; }'
assert $build 9 'int main() { int x[3][4]; return sizeof **x + 1; }'
assert $build 8 'int main() { int x[3][4]; return sizeof(**x + 1); }'
assert $build 8 'int main() { int x=1; return sizeof(x=2); }'
assert $build 1 'int main() { int x=1; sizeof(x=2); return x; }'

assert $build 0 'int x; int main() { return x; }'
assert $build 3 'int x; int main() { x=3; return x; }'
assert $build 7 'int x; int y; int main() { x=3; y=4; return x+y; }'
assert $build 7 'int x, y; int main() { x=3; y=4; return x+y; }'
assert $build 0 'int x[4]; int main() { x[0]=0; x[1]=1; x[2]=2; x[3]=3; return x[0]; }'
assert $build 1 'int x[4]; int main() { x[0]=0; x[1]=1; x[2]=2; x[3]=3; return x[1]; }'
assert $build 2 'int x[4]; int main() { x[0]=0; x[1]=1; x[2]=2; x[3]=3; return x[2]; }'
assert $build 3 'int x[4]; int main() { x[0]=0; x[1]=1; x[2]=2; x[3]=3; return x[3]; }'

assert $build 8 'int x; int main() { return sizeof(x); }'
assert $build 32 'int x[4]; int main() { return sizeof(x); }'

assert $build 1 'int main() { char x=1; return x; }'
assert $build 1 'int main() { char x=1; char y=2; return x; }'
assert $build 2 'int main() { char x=1; char y=2; return y; }'
assert $build 1 'int main() { char x; return sizeof(x); }'
assert $build 10 'int main() { char x[10]; return sizeof(x); }'
assert $build 7 'int sub_char(char a, char b, char c) { return a; } int main() { return sub_char(7, 3, 3); }'
assert $build 1 'int sub_char(char a, char b, char c) { return a-b-c; } int main() { return sub_char(7, 3, 3); }'

assert $build 97 'int main() { return "abc"[0]; }'
assert $build 98 'int main() { return "abc"[1]; }'
assert $build 99 'int main() { return "abc"[2]; }'
assert $build 0 'int main() { return "abc"[3]; }'
assert $build 4 'int main() { return sizeof("abc"); }'

echo OK