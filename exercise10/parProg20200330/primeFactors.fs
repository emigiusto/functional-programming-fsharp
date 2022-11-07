// F# parallel computing examples inspired by Hansen and Rischel
// chapter 13.6 * sestoft@itu.dk * 2013, 2017-03-20

let isPrime n =
    let rec testDiv a = a*a > n || n % a <> 0 && testDiv (a+1)
    n>=2 && testDiv 2;;

let factors n =
    let rec factorsIn d m =
        if m <= 1 then []
        else if m % d = 0 then d :: factorsIn d (m/d) else factorsIn (d+1) m
    factorsIn 2 n;;

let random n =
    let generator = new System.Random ()
    fun () -> generator.Next n;;

let r10000 = random 10000;; // 150x faster than creating a new System.Random

let rec ntimes (f : unit -> 'a) n =
    if n=0 then () else (ignore (f ()); ntimes f (n-1));;
    
let bigArray = Array.init 5000000 (fun _ -> r10000 ());;

#time;;

Array.map isPrime bigArray;;
// Real: 00:00:00.320, CPU: 00:00:00.319
Array.Parallel.map isPrime bigArray;;
// Real: 00:00:00.080, CPU: 00:00:00.617

// Better example: Prime factors of random numbers (more work)

Array.map factors bigArray;;
// Real: 00:00:22.076, CPU: 00:00:22.041
   
Array.Parallel.map factors bigArray;;
// Real: 00:00:06.538, CPU: 00:00:33.396
   
// Even better example: Prime factors of [1..200000]

Array.init 200000 factors;;
let factors200000 = Array.Parallel.init 200000 factors;;

// > Array.init 200000 factors;;
// Real: 00:00:11.650, CPU: 00:00:11.631

// > Array.Parallel.init 200000 factors;;
// Real: 00:00:02.490, CPU: 00:00:17.859

let histogram = Array.init 200000 (fun i -> 0)
let incr i = histogram.[i] <- histogram.[i] + 1
Array.iter (fun fs -> List.iter incr fs) factors200000;;

histogram;;
