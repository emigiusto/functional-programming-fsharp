module a9

(* Assignment 8.1, HR 9.8: *)
type BinTree<'a> = (* Page 133 *)
  | Leaf
  | Node of BinTree<'a> * 'a * BinTree<'a>


let rec countA t acc = 
  match t with
  | Leaf -> acc
  | Node(tl,n,tr) -> countA tl (countA tr (acc+1)) 

(* Example *)
let t = Node(Node(Leaf,3,Node(Leaf,3,Leaf)),1,Node(Leaf,4,Leaf))
countA t 0

(* Reorder *)
(*

let rec countB acc t = 
  match t with
  | Leaf -> acc
  | Node(tl,n,tr) -> countB (countB (acc+1) tr) tl 

*)

(* Assignment 8.2, HR 9.9 *)

let rec countAC t a c = 
  match t with
  | Leaf -> c a
  | Node(tl,n,tr) -> countAC tl (a+1) (fun r -> countAC tr (r) c)

(* Example *)
countAC t 0 id

(* Assignment 8.3, HR 9.10 *)
let rec bigListK n k =
  if n=0 then k []
  else bigListK (n-1) (fun res -> 1::k(res))

// bigListK 130000 id
// bigListK 300000 id

(* 
  Explanation of why it goes in overflow:

  Because every stack has a binding to n with a continuation function taking  
  
*)

(* Assignment 8.4, HR 9.11 *)

let rec leftTreeC n c =
    match n with
    | 0 -> Node(Leaf, n, Leaf)
    | n -> Node(leftTreeC (n-1) (fun r -> c (r)), n, Leaf)

let rec leftTree n =
  match n with 
  | 0 -> Node(Leaf, n, Leaf)
  | n -> Node(leftTreeC (n-1) (fun r -> leftTreeC (r)), n, Leaf)

(* Examples *)
leftTree 0
leftTree 1
leftTree 2
leftTree 3
leftTree 4
leftTree 1000
leftTree 360000

let rec rightTreeC n c =
  match n with
  | 0 -> Node(Leaf, n, Leaf)
  | n -> Node(Leaf, n, rightTreeC (n-1) (fun r -> c (r)))


let rightTree n =
  match n with 
  | 0 -> Node(Leaf, n, Leaf)
  | n -> Node(Leaf, n, rightTreeC (n-1) (fun r -> rightTreeC (r)))

(* Examples *)
rightTree 0
rightTree 1
rightTree 2
rightTree 3
rightTree 1000

let rec count = function (* from page HR 214 *)
    Leaf -> 0
  | Node(tl,n,tr) -> count tl + count tr + 1

let rec countC t c = (* from page HR 215 *)
  match t with
    Leaf -> c 0
  | Node(tl,n,tr) -> countC tl (fun vl -> countC tr (fun vr -> c(vl+vr+1)))

(* Assignment 8.5, HR 11.1 
Make a declaration for the sequence of odd numbers *)
let id (i : int) = i

let oddNumbers = Seq.filter (fun i -> i%2=1) (Seq.initInfinite id)

oddNumbers


Seq.take 0 oddNumbers
Seq.take 1 oddNumbers
Seq.take 2 oddNumbers
Seq.take 3 oddNumbers
Seq.take 10 oddNumbers


(* Assignment 8.6, HR 11.2 
Make a declaration for the sequence of numbers 1, 1, 2, 6, ..., n!, ... *)

let fac = Seq.map (fun i -> if i=0 || i=1 then 1 else (i*(i-1))) (Seq.initInfinite id)

(*Examples *)
Seq.take 0 fac
Seq.take 1 fac
Seq.take 2 fac
Seq.take 10 fac

