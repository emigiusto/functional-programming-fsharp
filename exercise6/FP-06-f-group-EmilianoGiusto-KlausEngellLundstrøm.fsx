module test
// 6.1 (HR 6.2)
(*
  Postfix form is a particular representation of arithmetic expressions where each operator is
  preceded by its operand(s), for example:
  (x + 7.0) has postfix form x 7.0 +
  (x + 7.0) ∗ (x − 5.0) has postfix form x 7.0 + x 5.0 − ∗
  Declare an F# function with type Fexpr -> string computing the textual, postfix form of
  expression trees from Section 6.2.
*)
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
    | Const(x) -> x.ToString()
    | X -> X.ToString()
    | Add(x,y) -> fexprToString(x) + " " + fexprToString(y) + " +"
    | Sub(x,y) -> fexprToString(x) + " " + fexprToString(y) + " -"
    | Mul(x,y) -> fexprToString(x) + " " + fexprToString(y) + " *"
    | Div(x,y) -> fexprToString(x) + " " + fexprToString(y) + " /"
    | Sin(x) -> fexprToString(x) + " sin"
    | Cos(x) -> fexprToString(x) + " cos"
    | Log(x) -> fexprToString(x) + " log"
    | Exp(x) -> fexprToString(x) + " exp"


// 6.2 (HR 6.8)

type Stack = S of float list

type Instruction = | ADD | SUB | MULT | DIV | SIN
                   | COS | LOG | EXP | PUSH of float


let getTwoElements (stack:Stack) = stack;

let intpInstr (stack: Stack) instruction =
  match instruction, stack with
    | (ADD, S(x::y::restOfStack)) -> S((x+y)::restOfStack)
    | (SUB, S(x::y::restOfStack)) -> S((y-x)::restOfStack)
    | (MULT, S(x::y::restOfStack)) -> S((x*y)::restOfStack)
    | (DIV, S(x::y::restOfStack)) -> S((y/x)::restOfStack)
    | (SIN, S(x::restOfStack)) -> S((sin(x))::restOfStack)
    | (COS, S(x::restOfStack)) -> S((cos(x))::restOfStack)
    | (LOG, S(x::restOfStack)) -> S((log(x))::restOfStack)
    | (EXP, S(x::restOfStack)) -> S((exp(x))::restOfStack)
    | (PUSH newFloat, S(restOfStack)) -> S(newFloat::restOfStack)


let intpProg listOfInstructions = 
    let rec eval listOfInstructions stack =
        match listOfInstructions with
        | x :: xs -> eval xs (intpInstr stack x)
        | [] -> stack
    match eval listOfInstructions (S []) with
    | S (x::xs) -> x 
    | S ([]) -> failwith "Stack is empty"

//let trans ... ????????

// 6.3 (HR 7.2)
//type ComplexNumber ...
