let infSeq3 = Seq.initInfinite (fun i -> i*3)
let infSeq3List = infSeq3 |> Seq.toList |>  Seq.take 10

Seq.take 3 infSeq3

let ut43 = Seq.toList <| Seq.take 10 infSeq3

type item = {id : int;
             name : string;
             price : float}

type register = item list

(* 1.1 Declare a value of type register with the following four items:
     a) Item with id 1 named "Milk" with price 8.75
     b) Item with id 2 named "Juice" with price 16.25
     c) Item with id 3 named "Rye Bread" with price 25.00
     d) Item with id 4 named "White Bread" with price 18.50
*)

let v = [{id=1; name="Milk";        price= 8.75};
         {id=2; name="Juice";       price=16.25};
         {id=3; name="Rye Bread";   price=25.00};
         {id=4; name="White Bread"; price=18.50}]