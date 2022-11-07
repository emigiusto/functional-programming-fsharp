// 2.1 time difference:
let timediff (h0, m0) (h1, m1) = (h1*60 + m1) - (h0*60 + m0) 
timediff (12,34) (11,35)

// 2.2 function minutes
let minutes (h, m) = timediff (0,0) (h,m)
minutes (23,1)

// 2.3 / HR 2.2
let rec pow (s,n) = 
    match n with
    | 0 -> ""
    | 1 -> s
    | n -> s + pow(s,n-1)
pow ("ha",3)

// 2.4 / HR 2.8
let rec bin (n,k) =
    match k with
    | 0 -> 1
    | k when n = k -> 1
    | k -> bin(n-1,k-1) + bin(n-1,k)
bin(4,2)

// 2.5 / HR 2.9
let rec f = function
| (0,y) -> y
| (x,y) -> f(x-1, x*y);;
// 1) int * int -> int
// 2) for x = 0
// 3) f(2,3) --> f(1,6) --> f(0,6) --> 6
//    f(5,2) --> f(4,10) --> f(3,40) --> f(2,120) --> f(1,240) --> 240
//      
// 4) 

// 2.6 / HR 2.10
let rec fact = function
| 0 -> 1
| n -> n * fact(n-1)
if false then fact -1 else 0
let test(c,e) = if c then e else 0;;
// 1) bool * int -> int
// 2) Runtime Error because fact(-1) is an infinite loop
test(false, fact(-1))


// 2.7 / HR 2.13 Curry and Uncurry
let curry f x y = f(x,y)

//uncurry : (’a -> ’b -> ’c) -> ’a * ’b -> ’c
let uncurry g (x,y) = g x y