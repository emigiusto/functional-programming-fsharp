module Programfs
let sqr (x : int) = x * x

let pow a b : float = System.Math.Pow(a,b);

// 1.3 / HR 1.1
let g n = n + 4

// 1.4 / HR 1.2
let h(x:float, y:float) = System.Math.Sqrt(x*x + y*y)

// 1.5 / HR 1.4
let rec f = function
    | 1 -> 1
    | n -> n + f (n-1)

// 1.6 / HR 1.5
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n-2) + fib (n-1)

// 1.7 / HR 1.6
let rec sum = function
  | (m,0) -> m
  | (m,n) -> m+n + sum(m, n-1)

// 1.8 / HR 1.7
//(System.Math.PI, fact -1) ----> float * int
//fact(fact 4) ----> int
//power(System.Math.PI, fact 2) ----> float
//(power, fact) ----> (float * int -> float) * (int -> int)


// 1.9 / HR 1.8
let a = 5;;
let f1 a = a + 1;;
let g1 b = (f1 b) + a;;

// 1.10 Duplicate strings: dup:string -> string
let dup a:string = "" + a + a

// 1.11 Duplicate string n times.
let dupn text n = String.replicate n text