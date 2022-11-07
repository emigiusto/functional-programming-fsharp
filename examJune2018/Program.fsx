(* 1.1 *)
type OrderedList<'a when 'a : equality> =
        {   front: 'a list;
            rear: 'a list
        }

let ex = {front = ['x']; rear = ['z';'y']}


(*  3 0
    0 3
    2 1
    1 2
    
    4 representations *)

(* 1.2 *)

let canonical ol =
    match ol with
    | {front= _; rear= []} -> ol
    | {front= f; rear=r} -> {front= f @ List.rev r; rear=[]} 

let toList ol = 
    match ol with
    | {front= []; rear= []} -> []
    | {front= f; rear= r} -> f @ List.rev r


(* 1.3 *)

let newOL() = {front= []; rear= []}

let isEmpty ol =
    match ol with
    | {front= []; rear= []} -> true
    | {front= _; rear= _} -> false
isEmpty (newOL())

(* 1.4 *)

let addFront element ol= {front= [element] @ ol.front; rear= ol.rear}

addFront 'w' ex |> toList

let ol1 = {front = ["Hans"; "Brian"; "Gudrun"]; rear = []}
let ol2 = {front = ["Hans"; "Brian"]; rear = ["Gudrun"]}
let ol3 = {front = ["Hans"]; rear = ["Gudrun"; "Brian"]}
let ol4 = {front = []; rear = ["Gudrun"; "Brian"; "Hans"]}
let ol5:OrderedList<string> = {front = []; rear = []}

let removeFront ol = 
    match ol with
    | {front= [] ; rear= []} -> failwith "Ordered list is empty"
    | {front= [] ; rear= r } -> (r.Head, {front=[]; rear=r.Tail})
    | {front= x::f; rear= r} -> (x, {front=f; rear= List.rev r})

removeFront (ol4)

let peekFront ol =
    match ol with
    | {front= [] ; rear= []} -> failwith "Ordered list is empty"
    | {front= [] ; rear= r } -> (List.rev r).Head
    | {front= f ; rear= _ } -> f.Head
peekFront (ol5)

(* 1.5 *)
let ex1 = {front = ['x']; rear = ['z';'y']}
let append ol1 ol2 = {front= ol1.front @ List.rev ol1.rear ; rear= ol2.rear @ List.rev ol2.front}
append ex1 ex1 |> toList

(* 1.6 *)
let map f ol =
    match ol with
    | {front= [] ; rear= []} -> failwith "Ordered list is empty"
    | {front= [] ; rear= r } -> {front=[]; rear= List.map f r}
    | {front= fr ; rear= [] } -> {front= List.map f fr; rear=[]}
    | {front= fr ; rear= r } -> {front= List.map f fr; rear= List.map f r}

let map2 f ol = {front= List.map f ol.front; rear= List.map f ol.rear}

map2 (fun e -> e.ToString()) ex

(* 1.7 *)

let fold f acc ol  = List.fold f acc (toList (ol))
fold (fun acc e -> acc + e.ToString()) "" ex


let ex3 = addFront 'x' ex1
(* 1.8 *)
let multiplicity ol = 
    let folded = fold (fun acc e -> acc @ [e]) [] ol
    let mult list map =
        let x::listx = list
        match Map.tryFind x map with
        | Some i -> map |> Map.add x (i+1)
        | None -> map |> Map.add x 1
    let multi = mult folded Map.empty
    1
   
    
let multiplicity2 ol = 
  let addMult m e = 
    match Map.tryFind e m with
      Some i -> Map.add e (i+1) m
    | None -> Map.add e 1 m
  fold addMult Map.empty ol
let p1_24 = multiplicity (addFront 'x' ex)