module test
// 6.1 (HR 6.2)
type Fexpr = 
    | Const of float
    | X
    | Add of Fexpr * Fexpr
    | Sub of Fexpr * Fexpr
    | Mul of Fexpr * Fexpr
    | Div of Fexpr * Fexpr
    | Sin of Fexpr
    | Cos of Fexpr
    | Log of Fexpr
    | Exp of Fexpr;;

let rec fexprToString expr =
    match expr with
    | Const x -> string x 
    | X -> string expr 
    | Add (x, y) -> sprintf"%s %s +" (fexprToString x) (fexprToString y)
    | Sub (x, y) -> sprintf"%s %s -" (fexprToString x) (fexprToString y)
    | Mul (x, y) -> sprintf"%s %s *" (fexprToString x) (fexprToString y)
    | Div (x, y) -> sprintf"%s %s /" (fexprToString x) (fexprToString y)
    | Sin (x) -> sprintf"%s sin" (fexprToString x)
    | Cos (x) -> sprintf"%s cos" (fexprToString x)
    | Log (x) -> sprintf"%s log" (fexprToString x)
    | Exp (x) ->  sprintf"%s exp" (fexprToString x)

// 6.2 (HR 6.8)

type Stack = S of float list

type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP | PUSH of float


let intpInstr s inst =
    match s with
    | S (s::s1::sx) -> match inst with
                    | ADD -> S ((s1+s)::sx)
                    | SUB -> S ((s1-s)::sx)
                    | MULT -> S ((s1*s)::sx)
                    | DIV -> S ((s1/s)::sx)
    | S (s::sx) -> match inst with
                    | SIN -> S (System.Math.Sin(s)::sx)
                    | COS -> S (System.Math.Cos(s)::sx)
                    | LOG -> S (System.Math.Log(s)::sx)
                    | EXP -> S (System.Math.Exp(s)::sx)
                    | PUSH r -> S (r::s::sx)
    | S (s) -> match inst with
               | PUSH r -> S (r::s)              


let intpProg inst = 
    let rec eval inst s =
        match inst with
        | x :: xs -> eval xs (intpInstr s x)
        | [] -> s
    match eval inst (S []) with
    | S (x::xs) -> x 
    | S ([]) -> failwith "stack failure"

/// Test
/// Name: intpInstr_given_5_5_5_mult_mult_returns_125
printfn "%A" (intpProg [PUSH 5.;PUSH 5.;PUSH 5.;MULT;MULT])


//let trans ...

// 6.3 (HR 7.2)
// !!
//this will not compile as it should be separate files, however for the hand-in we put it in one file 

// module complex
type ComplexNumber
val ( .+ ) : ComplexNumber * ComplexNumber -> ComplexNumber 
val ( .* ) : ComplexNumber * ComplexNumber -> ComplexNumber 
val ( .- ) : ComplexNumber * ComplexNumber -> ComplexNumber 
val ( ./ ) : ComplexNumber * ComplexNumber -> ComplexNumber 

val make : (float * float) -> ComplexNumber 

type ComplexNumber = C of (float * float)
    let ( .+ ) (C(a,b)) (C(c,d)) = C(a+c,b+d)
    let ( .* ) (C(a,b)) (C(c,d)) = C(a*c - b*d, b*c + a*d)
    let ( .- )  (C(a,b)) (C(c,d)) = C((a-c,b-d))
    let ( ./ ) (C(a,b)) (C(c,d)) = C((a*c + b*d)/(c**2.0 + d**2.0),(b*c - a*d)/(c**2.0+d**2.0))

let make (a,b) = C(a,b)

//for sealed
// module complex2
[<Sealed>]
type ComplexNumber =
static member ( + ) : ComplexNumber * ComplexNumber -> ComplexNumber
static member ( * ) : ComplexNumber * ComplexNumber -> ComplexNumber
static member ( - ) : ComplexNumber * ComplexNumber -> ComplexNumber
static member ( / ) : ComplexNumber * ComplexNumber -> ComplexNumber
val make : (float * float) -> ComplexNumber

type ComplexNumber =
    | C of (float * float)
    static member (+) (C(a,b)) (C(c,d)) = C(a+c,b+d)
    static member (*) (C(a,b)) (C(c,d)) = C(a*c - b*d, b*c + a*d)
    static member (-) (C(a,b)) (C(c,d)) = C((a-c,b-d))
    static member (/) (C(a,b)) (C(c,d)) = C((a*c + b*d)/(c**2.0 + d**2.0),(b*c - a*d)/(c**2.0+d**2.0))
let make (x,y) = C(x,y)

let no = make (1,2)
let no2 = make (2,3)


// let c = 2.0 * no - no2;;
