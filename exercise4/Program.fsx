// Exercise 4.1 - explode - string to char list
let explode (s:string) =  List.ofArray(s.ToCharArray())
explode "string"

let rec explode2 (s:string) = 
    if s <> ""
    then s.[0] :: explode2 (s.Remove(0,1))
    else []
//explode2 "string"

// Exercise 4.2 - implode - char list to string
//ASK HOW TO TO list<char> -> string
let implodeString list = (list, "") ||>List.foldBack(fun s1 s2 -> s1 + "" + s2)
implodeString ["1";"2";"3"]

let implodeRevString list = List.fold(fun s1 s2-> s2 + "" + s1) "" list
implodeRevString ["1";"2";"3";"4"]

//Solved using arrays and not FOLD
let (..+) (a:char) (b:string) = System.Char.ToString(a) + b
let implode (list:list<char>) = List.foldBack (..+) list ""
implode ['1';'2';'3']

let (+..) (b:string)  (a:char) = System.Char.ToString(a) + b
let implodeRev (list:list<char>) =  List.fold (+..) "" list
implodeRev ['1';'2';'3']

// Exercise 4.3 - toUpper
let toUpper (s:string) = explode s |> List.map System.Char.ToUpper |> fun list -> implode list
//toUpper "string"

// explode >> map >> implode
let toUpper1 (s:string) = (explode >> List.map System.Char.ToUpper >> implode) s
toUpper1 "string"

//let toUpper2 (s:string) = implode (explode s |> List.map(fun x -> (System.Char.ToUpper(x))));;
let toUpper2 (s:string) = explode s |> (implode << List.map(fun x -> (System.Char.ToUpper(x))))
toUpper2 "string"

//let invertString (s:string) = 
// Exercise 4.4 - palindrome - treating empty strings as palindromes too.
let rec palindrome (s1:string) = 
    let s = toUpper s1
    match s with
    | "" -> true
    | _ when s.Length = 1 -> true
    | _ when (s.[0] = s.[s.Length-1]) && palindrome(s.[1..s.Length-2]) -> true
    | _ -> false
palindrome "ertRe"

// Exercise 4.5 - ack
let rec ack t = 
    match t with
    | (0,n) -> (n+1)
    | (m,0) when m >0 -> ack(m-1,1)
    | (m,n) when m >0 && n>0 -> ack(m-1,ack(m,n-1))
ack(3, 11)

// Exercise 4.6 - time
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start);
//time (fun () -> ack (3,11))

let timeArg1 funct a = time(fun () -> funct a)
timeArg1 ack (3,11)


// Exercise 4.7 - HR 5.4 - downTo2 f n e
let rec downto1 f (n, e) =
    match n with
    | n when n <= 0 -> e
    | _ -> downto1 f (n-1, f(n, e))

downto1 (fun (v,e)->v+e) (1, 1)

// factorial function using downto1 for recursion.
let fact n = 
    match n with
    | 0 -> 1
    | n when n > 0 -> downto1 (fun (v,t) -> v*t) (n,1)
    | _ -> failwith "fact only works on positive numbers"
fact 3

//downto1 (fun (r,v)->fact r::v)

let buildList g n = downto1 (fun x y -> g(x)::y) (n, [])

//buildList fact 2




//Expected outcome: [g(1), g(2), . . . , g(n)]
//-> g(x)::xs