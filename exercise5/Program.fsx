type 'a BinTree =
    Leaf
  | Node of 'a * 'a BinTree * 'a BinTree

let intBinTree = 
  Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
                    Node(562, Leaf, Node(78, Leaf, Leaf)))

let rec countNodes tree =
  match tree with
    Leaf -> (1,0)
  | Node(_,treeL, treeR) ->
      let (ll,lr) = countNodes treeL in
      let (rl,rr) = countNodes treeR in
      (ll+rl,1+lr+rr);

let floatBinTree = 
  Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),
            Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))

let rec preOrder tree =
  match tree with
    Leaf -> []
  | Node(n,treeL,treeR) ->
      n :: preOrder treeL @ preOrder treeR;

// 5.1
let rec inOrder tree =
  match tree with
    Leaf -> []
  | Node(n,treeL,Leaf) -> inOrder treeL @ [n];
  | Node(n,Leaf,treeR) -> n :: inOrder treeR;
  | Node(n,treeL,treeR) -> inOrder treeL @ [n] @ inOrder treeR;

inOrder intBinTree
//val it : int list = [56; 25; 43; 562; 78]

//let mapInOrder1 g tree  = List.map g (inOrder tree) --> Returns a list --> wrong
// 5.2
let rec mapInOrder g tree  = 
    match tree with
    | Leaf ->Leaf
    | Node(n,treeL,treeR) -> 
      let treeL = mapInOrder g treeL
      Node(g n, treeL,mapInOrder g treeR)


// 5.3
let rec foldInOrder g initial tree = 
  match tree with 
    | Leaf -> initial 
    | Node(n,treeL,Leaf) -> foldInOrder g (g n initial) treeL
    | Node(n,Leaf,treeR) -> g n (foldInOrder g initial treeR)
    | Node(n,treeL,treeR) -> 
      let leftSide = g (foldInOrder g initial treeL) n 
      g leftSide (foldInOrder g initial treeR)


//List.fold g initial (inOrder tree)

//let floatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))
foldInOrder (fun n a -> a + n) 0.0 floatBinTree
//This evaluates to 764.0 as expected but types are wrong. Should be polymorfic
//foldInOrder : (’a -> ’b -> ’b) -> ’b -> ’a BinTree -> ’b
//And its:
//foldInOrder : (’a -> float -> float) -> ’b -> ’a BinTree -> ’b

/// Test
/// Name: foldInOrder_given_'int_tree_into_list'_func_returns_inorder_list
let cjTestTree4 = 
    Node (0,Node (3,Node (2,Leaf,Leaf),Node (6,Leaf,Leaf)),
        Node (9,Node (4,Leaf,Leaf),Node (5,Leaf,Leaf)));;
let cjFunc4 = (fun x acc -> x :: acc);;
printfn "%A" (List.rev <| foldInOrder cjFunc4 [] cjTestTree4);;
/// Test
/// Name: foldInOrder_given_'char-into-list'_func_returns_inorder_list
let cjTestTree5 = Node('e',Node ('c',Node ('b',Leaf,Leaf),Node ('d',Leaf,Leaf)),Node ('g',Node ('f',Leaf,Leaf),Node ('h',Leaf,Leaf)))
let cjFunc5 = (fun x acc -> x :: acc)
printfn "%A" (List.rev <| foldInOrder cjFunc5 [] cjTestTree5)
/// Test
/// Name: foldInOrder_given_floatBinTree_and_'add'_func_returns_sum
let cjFloatBinTree = Node(43.0,Node(25.0, Node(56.0,Leaf, Leaf), Leaf),Node(562.0, Leaf, Node(78.0, Leaf,Leaf)))
printfn "%A" (foldInOrder (fun n a -> a + n) 0.0 cjFloatBinTree)

//5.4 + 5.5
type aExp =                     (* Arithmetical expressions *)
    | N of int                  (* numbers *)
    | V of string               (* variables *)
    | Add of aExp * aExp        (* addition *)
    | Mul of aExp * aExp        (* multiplication *)
    | Sub of aExp * aExp        (* subtraction *)

type bExp =                     (* Boolean expressions *)
    | TT                        (* true *)
    | FF                        (* false *)
    | Eq of aExp * aExp         (* equality *)
    | Lt of aExp * aExp         (* less than *)
    | Neg of bExp               (* negation *)
    | Con of bExp * bExp        (* conjunction *)

type stm =                      // statements
    | Ass of string * aExp      // assignment
    | Skip
    | Seq of stm * stm          // sequential composition
    | ITE of bExp * stm * stm   // if-then-else
    | While of bExp * stm       // while
    | IT of bExp * stm          // if-then
    | RU of stm * bExp          //  repeat until

let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;

let rec B b s =
  match b with
      | TT -> true
      | FF -> false
      | Eq(V key,N value) -> (s |>Map.find key =  value);
      | Lt(V key,N value) -> (s |> Map.find key <  value);
      | Neg(TT) -> false;
      | Neg(FF) -> true;
      | Con(TT, TT) -> true;
      | _ -> false;



let rec I stm s =
    match stm with
    | Ass(x,a) -> s |> Map.add  x (A a s);
    | Skip -> s;
    | Seq(first,second) -> I first s |> I second
    | ITE(condition,firstExp,secondExp) ->  if B condition s then I firstExp s else I secondExp s;
    | IT(condition,expression) ->  if B condition s then I expression s else s;
    | While(condition,expression) -> if B condition s then I expression s else s;
    | RU(expression,condition ) -> if B condition s then s else I expression s;