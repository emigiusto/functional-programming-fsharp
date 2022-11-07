module a2
// Exercise 3.1 downTo + downTo2
let rec downTo n =
    if n > 0
    then n :: downTo (n-1)
    else []


let rec downTo2 n = 
    match n with
    | 0 -> []
    | m -> m::downTo2(m-1)

// Exercise 3.2 removeOddIdx
let rec removeOddIdx (xs: int list) = 
    match xs with
    | [] -> []
    | [x] -> [x]
    | x::y::xs -> x::removeOddIdx (xs)

// Exercise 3.3 combinePair
let rec combinePair (xs :int list) = 
    match xs with
    | [] -> []
    | [x] -> []
    | x::y::xs -> (x,y)::combinePair(xs)
combinePair(downTo2 5)

// Exercise 3.4 - HR 3.2 - British currency
// The former British currency had 12 pence to a shilling and 20 shillings to a pound. Declare
// functions to add and subtract two amounts, represented by triples (pounds, shillings, pence) of
// integers, and declare the functions when a representation by records is used. Declare the functions
// in infix notation with proper precedences, and use patterns to obtain readable declarations.

// Money tuple addition
let (^+^) a b = 
    match a,b with
    | (po1,sh1,pe1),(po2,sh2,pe2) -> (po1+po2 + ((sh1+sh2)%20 + (pe1+pe2)/12)/20,(sh1+sh2)%20 + (pe1+pe2)/12 ,(pe1+pe2)%12) 

(1,2,12)^+^(1,2,3)

// Money tuple subtraction
let (^-^) a b = 
    match a,b with
    | (po1,sh1,pe1),(po2,sh2,pe2) -> (((po1+sh1*20+pe1*12)-(po2+sh2*20+pe2*12))/240, (((po1+sh1*20+pe1*12)-(po2+sh2*20+pe2*12))/12)+(((po1+sh1*20+pe1*12)-(po2+sh2*20+pe2*12))%240), ((po1+sh1*20+pe1*12)-(po2+sh2*20+pe2*12))+ ((((po1+sh1*20+pe1*12)-(po2+sh2*20+pe2*12))%12)+(((po1+sh1*20+pe1*12)-(po2+sh2*20+pe2*12))%240))/20)
(2,4,5)^-^(2,2,13)


type Money = {pound : int; shilling : int; pence : int};;
// Money record addition
let (|+|) a b = failwith "not implemented"
// Money record subtraction
let (|-|) a b = failwith "not implemented"

// Exercise 3.5 - HR 3.3 - Complex numbers

//      1. Declare infix for addition and multiplication
let ( .+) (a:float,b:float) (c:float,d:float) = failwith "not implemented"

let ( .*) (a:float,b:float) (c:float,d:float) = failwith "not implemented"

//      2. Declare infix for subtraction and division
let ( .-) (a:float,b:float) (c:float,d:float) = failwith "not implemented"

let ( ./) (a,b) (c,d) = failwith "not implemented"
//      3. Use 'let' expressions in division to avoid repeated evals
let ( ../) (a:float,b:float) (c:float,d:float) = failwith "not implemented"


// Exercise 3.6 - HR 4.4 - altSum -> HR page 76
// function alternating between adding and subtracting the contents of a list.
//let rec altsum = failwith "not implemented";;