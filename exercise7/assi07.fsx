module a8

(* Assignment 7.1, HR 9.1 *)
(* Not covered by Code Judge *)
let xs = [1;2]

let rec g = function
    0 -> xs
  | n -> let ys = n::g(n-1)
         List.rev ys
g 2

(* Draw the stack  *)

(* Assignment 7.2, HR 9.3 *)

let rec sum(m,n) = 
  let rec i n x = 
    match n with
    | 0 -> x
    | n -> i (n-1) (x + (m+n))
  i n m 

(* Example *)
sum(10,10)

(* Assignment 7.3, HR 9.4 *)

let length xs =
  let rec tail xs counter = 
    match xs with
    | [] -> counter
    | xs -> tail xs.Tail (counter+1)
  tail xs 0

let list1 = [ 2; 4; 6; 8; 10; 12; 14; 16 ]
length list1

(* Example *)
length [1]


(* Assignment 7.4, HR 9.6 *)
let rec facC n c = 
  match n with
    | 0 -> c 1 
    | n -> facC (n-1) (fun r -> c(r*n))  

(* Example *)
facC 5 id

(* Assignment 7.5, HR 8.6 *)
(* I command you, get pusheeeddddd *)
let fib n = 
  let mutable fibValue = 0
  if n > 0 then fibValue <- fibValue + 1 
  let mutable i = 1
  let mutable n_2 = 0
  let mutable n_1 = 1
  let mutable cond = i<n
  while cond do
    let mutable addValue = n_1 + n_2
    fibValue <- addValue
    n_2 <- n_1
    n_1 <- addValue
    addValue <- n_1 + n_2
    i <- i + 1
    if i >= n then cond <- false
  fibValue

(* Example *)
fib 7

(* Assignment 7.6, HR 9.7 *)

let fibA n n1 n2 =
  let rec acc n n1 n2 i =
    if i < n then acc n (n1 + n2) n1 (i+1) else n1
  acc n n1 n2 0 

(* Example *)
fibA 10 0 1


let rec fibC n c = 
  match n with
    | 0 -> c 0
    | 1 -> c 1
    | n -> fibC (n-1) (fun r -> fibC (n-2) (fun x -> c (r + x)))

(* Example *)
fibC 10 id

