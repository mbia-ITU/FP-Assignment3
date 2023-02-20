type aExp =
    | N of int // Integer value
    | Add of aExp * aExp // Addition
    | Sub of aExp * aExp // Subtraction
    | Mul of aExp * aExp // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

//Tests
let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6);;
let a3 = N 4 .*. N 2 .+. N 34;;
let a4 = (N 4 .+. N 2) .*. N 34;;
let a5 = N 4 .+. (N 2 .*. N 34);;

//Exercise 3.1
let rec arithEvalSimple a =
    match a with 
    | N n -> n
    | Add (a,b) -> (arithEvalSimple a) + ( arithEvalSimple b)
    | Sub (a,b) -> arithEvalSimple a - arithEvalSimple b
    | Mul (a,b) -> arithEvalSimple a * arithEvalSimple b

//Exercise 3.2

