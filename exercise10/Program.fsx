(*
IT UNIVERSITY KSD FUNCTIONAL PROGRAMMING SPRING 2022
Assignment 10
These exercises concern parallel programming in F#.
You should build on the files in the example code, found in file
parProg20200330.zip

Exercise 1. Run the slow Fibonacci computations from the lecture's
examples on your own computer.  Use the #time directive to turn on
timing, and see what is the best speed-up you can obtain for
computing, say, the first 43 Fibonacci numbers using Async.Parallel.
This may be quite different on MS .NET than on Mono.
*)
let rec slowfib n = if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2);;

#time;;
let fibsSlow = [ for i in 0..45 do yield slowfib(i) ];;
#time;;
//Real: 00:00:13.768, CPU: 00:00:13.734, GC gen0: 0, gen1: 0, gen2: 0

#time;;
let fibsParallel =
    let tasks = [ for i in 0..45 do yield async { return slowfib(i) } ]
    Async.RunSynchronously (Async.Parallel tasks);;
#time;;
//Real: 00:00:06.302, CPU: 00:00:17.859, GC gen0: 0, gen1: 0, gen2: 0
//  7.4 secs in speedup, -54% reduction

(*
Exercise 2. Similarly, run the prime factorization example on your own
computer, and see what speedup you get.
*)

let factors n =
    let rec factorsIn d m =
        if m <= 1 then []
        else if m % d = 0 then d :: factorsIn d (m/d)
        else factorsIn (d+1) m
    factorsIn 2 n;;
#time;;
Array.init 200000 factors;
#time;;
//Real: 00:00:04.435, CPU: 00:00:04.437, GC gen0: 5, gen1: 2, gen2: 0

#time;;
Array.Parallel.init 200000 factors;;
#time;;
//Real: 00:00:01.253, CPU: 00:00:08.328, GC gen0: 4, gen1: 1, gen2: 0
// -71% reduction

(*
Exercise 3. The lecture's construction of a histogram (counting the
numbers of times that each prime factor 2, 3, 5, 7, 11 ... appears)
uses a side effect in the assignment
histogram.[i] <- histogram.[i] + 1 

    But side effects should be avoided.  Program the histogram
    construction so that it does not use side effects but purely
    functional programming.  There are several useful functions in the Seq
    module.  The final result does not have to be an int[] array, but could be a seq<int * int>
    of pairs (p, n) where p is a prime factor and n is the number of times p appears in the array of lists of prime factors.
*)
let factors200000 = Array.Parallel.init 200000 factors;;
let histogram = factors200000 |> Seq.concat |> Seq.groupBy id 
                    |> Seq.map (fun (key, values)-> (key,Seq.length values))

(*
Exercise 4. Find the fastest way on your hardware to count the number
of prime numbers between 1 and 10 million (the correct count is
664579).  Use this F# function to determine whether a given number n
is prime:

let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;

or if you believe it would be faster in C, C# or Java, you may use this version:

private static boolean isPrime(int n) {
    int k = 2;
    while (k * k <= n && n % k != 0)
        k++;
    return n >= 2 && k * k > n;
}

Remember to use parallel programming to the extent possible.
*)

let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;


let a = seq {for n in 1 .. 10000000 do if isPrime n then n }
#time;;
a |> List.ofSeq
#time;;
//Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0
//Why are the readings 0??

#time;;
let primes =
    let tasks = [ for i in 1..10000000 do if isPrime i then async {return i} ]
    Async.RunSynchronously (Async.Parallel tasks);;

    //Array.Parallel.map
#time;;
//Real: 00:00:03.471, CPU: 00:00:05.375, GC gen0: 109, gen1: 8, gen2: 2