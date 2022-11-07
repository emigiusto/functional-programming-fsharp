type Heap<'a when 'a: equality> =
    | EmptyHP
    | HP of 'a * Heap<'a> * Heap<'a>

//let empty:  = EmptyHP;;
//let empty : Heap<'a> when 'a : equality = EmptyHP
let empty = EmptyHP
let ex3 = HP(4,empty, HP(2,empty,HP(3,empty,empty)))

//Declare an F# exception named HeapError that can be used to signal an error condition from a
// function on heaps. The exception should carry a string to be used to describe the error.

exception HeapError of string

raise (HeapError("Child cannot be higher than parent"))

(*
    isEmpty : Heap<’a> -> bool when ’a : equality
    that returns true if a heap is the empty heap. For instance isEmpty empty returns true. The
    value empty is defined above.
*)

let isEmpty tree =
    match tree with
    | EmptyHP -> true
    | _ -> false

isEmpty empty
isEmpty ex3

(*
    The size h of a heap h is the number of non–empty nodes in the binary tree. Declare a function
    size : Heap<’a> -> int when ’a : equality
    that returns the size of a heap. For instance, size ex3 returns 5.
*)

let rec size tree = 
    match tree with
    | EmptyHP -> 0
    | HP(x,leftBrach,rightBranch) -> 1 + size leftBrach + size rightBranch

size empty
size ex3

(*
    Declare a function find h of type
    find : Heap<’a> -> ’a when ’a : equality
    that returns the minimum value in a non–empty heap, i.e. the root value. For instance find ex3
    returns 1.
*)
