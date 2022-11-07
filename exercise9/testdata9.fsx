let expr10 = Prim1("ABS", Prim("+", CstI(7), Prim("*", CstI(-9), CstI(10))))
let expr11 = Prim1("ABS", Prim("/", CstI(5), Prim("/", CstI(9), CstI(0))))
let expr12 = Prim("+", CstI(7), Prim("choose", Prim1("ABS", CstI(-9)), CstI(10)))

let oneof x xs = List.exists ((=)x) xs

///Test
///Name: Eval1_expr10
printfn "%A\n" (eval1 expr10)

///Test
///Name: Eval1_expr11
printfn "%A" (try eval1 expr11 with _ -> -1)

///Test
///Name: Eval1_expr12
printfn "%A" (oneof (eval1 expr12) [16;17])

///Test
///Name: Eval2_expr10
printfn "%A" (eval2 expr10)

///Test
///Name: Eval2_expr11
printfn "%A" (try eval2 expr11 with _ -> -1)

///Test
///Name: Eval2_expr12
printfn "%A" (oneof (eval2 expr12) [16;17])

///Test
///Name: Eval3_expr10
printfn "%A" (eval3 expr10)

///Test
///Name: Eval3_expr11
printfn "%A" (try eval3 expr11 with _ -> -1)

///Test
///Name: Eval3_expr12
printfn "%A" (oneof (eval3 expr12) [16;17])
