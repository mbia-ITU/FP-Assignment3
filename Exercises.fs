type aExp =
    | N of int // Integer value
    | Add of aExp * aExp // Addition
    | Sub of aExp * aExp // Subtraction
    | Mul of aExp * aExp // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

//Exercise 3.1
let rec arithEvalSimple (a: aExp) =
    match a with 
    | N -> N
    | Add (a,b) -> arithEvalSimple a + arithEvalSimple b
    | Sub (a,b) -> arithEvalSimple a - arithEvalSimple b
    | Mul (a,b) -> arithEvalSimple a * arithEvalSimple b


