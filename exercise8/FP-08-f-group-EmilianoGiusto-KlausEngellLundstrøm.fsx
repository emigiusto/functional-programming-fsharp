(* Assignment 8.1, HR 9.8 *)
type BinTree<'a> = (* Page 133 *)
    Leaf
  | Node of BinTree<'a> * 'a * BinTree<'a>


(* Develop a version of the counting function for binary trees countA: int -> BinTree<’a> -> int 
  that makes use of an accumulating parameter. Observe that this function is not tail recursive. *)
let rec countA t n = 
  match t with
    | Leaf -> 0
    | Node(nodeL , x ,Leaf) -> n + countA nodeL 0 + 1
    | Node(Leaf , x ,nodeR) -> n + countA nodeR 0 + 1
    | Node(nodeL , x ,nodeR) -> n + countA nodeL 0 + countA nodeR 0 + 1

(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
let s = Leaf
countA t 0

(* Assignment 8.2, HR 9.9
  Declare a tail-recursive functions with the type countAC : BinTree<’a> -> int -> (int -> ’b) -> ’b
    such that count t = countAC t 0 id. The intuition with countAC t a c is that a is the
    number of nodes being counted so far and c is the continuation.
*)
let rec countAC t a c = 
    match t with 
    | Leaf -> c a
    | Node(nodeL,x,nodeR) -> countAC nodeL (a+1)  (fun r -> countAC nodeR (r) c)

(* Example *)
countAC t 0 id

(* Assignment 8.3, HR 9.10 *)
(* 
  Consider the following list-generating function:
  let rec bigListK n k =
    if n=0 then k []
    else bigListK (n-1) (fun res -> 1::k(res));;

  The call bigListK 130000 id causes a stack overflow. Analyze this problem.
*)
let rec bigListK n k =
  if n=0 then k []
  else bigListK (n-1) (fun res -> 1::k(res))
(* Conclusion: Even though the function uses tail-recursion, therefore heap space instead of stack space, 
    the heap resources assigned to the dotnet terminal are limited *)

(* Assignment 8.4, HR 9.11 *)
(*
  Declare tail-recursive functions leftTree and rightTree. By use of leftTree it should
  be possible to generate a big unbalanced tree to the left containing n+1 values in the nodes so
  that n is the value in the root, n − 1 is the value in the root of the left subtree, and so on. All
  subtree to the right are leaves. Similarly, using rightTree it should be possible to generate a
  big unbalanced tree to the right.
  1. Use these functions to show the stack limit when using count and countA from Exercise
  9.8.
  2. Use these functions to test the performance of countC and countAC from Exercise 9.9.
*)
let rec leftTreeC n c = 
  match n with 
    | 0 -> Node(Leaf,0,Leaf) |> c
    | x -> leftTreeC (x-1) (fun res -> Node(res,x,Leaf) |> c )
leftTreeC 2 id
(*
  1) id
  2) id(id())
  3) id(id(id()))
*)

let rec leftTree n = 
  leftTreeC n id

leftTree 2

(* Examples *)
leftTree 0
leftTree 1
leftTreeC 360000 id

let rec rightTreeC n c =
  match n with 
    | 0 -> c (Node(Leaf,0,Leaf))
    | x -> rightTreeC (x-1) (fun res -> c(Node(Leaf,x,res)))
rightTreeC 2 id

let rightTree n = 
  rightTreeC n id

(* Examples *)
rightTree 0
rightTree 1
rightTree 2
rightTree 360000

let rec count = function (* from page HR 214 *)
    Leaf -> 0
  | Node(tl,n,tr) -> count tl + count tr + 1

let rec countC t c = (* from page HR 215 *)
  match t with
    Leaf -> c 0
  | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))

countC (rightTree 100000) id
count (rightTree 10000)


(* Assignment 8.5, HR 11.1 *)

let nat = Seq.initInfinite (fun i -> i); //Returns natural numbers
let oddNumbers = Seq.filter (fun n -> n%2<>0) (Seq.initInfinite (fun i -> i));;

Seq.take 3 oddNumbers

(* Assignment 8.6, HR 11.2 *)
(* Make a declaration for the sequence of numbers 1, 1, 2, 6, . . . , n!, . . .. *)
let fac = Seq.map (fun i -> if i=0 || i=1 then 1 else (i*(i-1))) (Seq.initInfinite id)



(*Examples *)
Seq.take 0 fac
Seq.take 1 fac
Seq.take 2 fac
Seq.take 10 fac