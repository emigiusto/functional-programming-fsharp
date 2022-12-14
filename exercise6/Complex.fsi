module Complex
[<Sealed>]
type ComplexNumber =
    static member ( + ) : ComplexNumber * ComplexNumber -> ComplexNumber
    static member ( * ) : ComplexNumber * ComplexNumber -> ComplexNumber
    static member ( - ) : ComplexNumber * ComplexNumber -> ComplexNumber
    static member ( / ) : ComplexNumber * ComplexNumber -> ComplexNumber
val make : (float * float) -> ComplexNumber