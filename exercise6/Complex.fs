module Complex
type ComplexNumber =
    | C of float * float
    static member (+) (C(a,b)) (C(c,d)) = C(a+c,b+d)
    static member (-) (C(a,b)) (C(c,d)) = C((a-c,b-d))
    static member (/) (C(a,b)) (C(c,d)) = C((a*c + b*d)/(c**2.0 + d**2.0),(b*c - a*d)/(c**2.0+d**2.0))
    static member (*) (C(a,b)) (C(c,d)) = C(a*c - b*d, b*c + a*d)
let make (y,z) = C(y,z)