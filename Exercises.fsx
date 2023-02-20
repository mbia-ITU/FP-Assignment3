open System

type aExp =
    | N of int // Integer value
    | V of string // Variable
    | WL // Length of the word
    | PV of aExp // Point value of character at specific word index
    | Add of aExp * aExp // Addition
    | Sub of aExp * aExp // Subtraction
    | Mul of aExp * aExp // Multiplication

type cExp =
    | C of char (* Character value *)
    | ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
    | ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
    | CV of aExp (* Character lookup at word index *)

type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)
    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)
    

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)
let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)
let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

type word = (char * int) list

//Tests
let a1 = N 42;;
let a2 = N 4 .+. (N 5 .-. N 6);;
let a3 = N 4 .*. N 2 .+. N 34;;
let a4 = (N 4 .+. N 2) .*. N 34;;
let a5 = N 4 .+. (N 2 .*. N 34);;
let a6 = V "x";;
let a7 = N 4 .+. (V "y" .-. V "z");;
let hello : word = [('H', 4); ('E',1);('L',1);('L',1);('O',1)]


//Exercise 3.1
let rec arithEvalSimple a =
    match a with 
    | N n -> n
    | Add (a,b) -> (arithEvalSimple a) + ( arithEvalSimple b)
    | Sub (a,b) -> arithEvalSimple a - arithEvalSimple b
    | Mul (a,b) -> arithEvalSimple a * arithEvalSimple b

//Exercise 3.2
let rec arithEvalState a  (s: Map<string, int>)= 
    match a with
    | N n -> n
    | V str -> Map.tryFind str s |> Option.defaultValue 0
        (*match Map.tryFind str s with
        | Some sme-> sme
        | None -> 0 *)
    | Add(a,b) -> arithEvalState a s + arithEvalState b s
    | Sub(a,b) -> arithEvalState a s - arithEvalState b s
    | Mul(a,b) -> arithEvalState a s * arithEvalState b s

//Exercise 3.3
let rec arithEval a (w: word) (s: Map<string, int>) = 
    match a with
    | N n -> n
    | V str -> Map.tryFind str s |> Option.defaultValue 0
    | WL -> w.Length
    | PV x -> snd w.[arithEval x w s]
    | Add(a,b) -> arithEval a w s + arithEval a w s
    | Sub(a,b) -> arithEval a w s - arithEval a w s
    | Mul(a,b) -> arithEval a w s * arithEval a w s

//Exercise 3.4
let rec charEval c (w: word) (s: Map<string, int>) =
    match c with
    | C c -> c
    | ToUpper c -> Char.ToUpper (charEval c w s)
    | ToLower c -> Char.ToLower (charEval c w s)
    | CV c -> fst w.[arithEval c w s]

//Exercise 3.5
let rec boolEval b (w: word) (s: Map<string, int>) = 
    match b with
    | TT -> true
    | FF -> false

    | AEq(a1,a2) -> arithEval a1 w s = arithEval a2 w s
    | ALt(a1,a2) -> arithEval a1 w s < arithEval a2 w s

    | Not(b1) -> not (boolEval b1 w s)
    | Conj(b1, b2) -> boolEval b1 w s && boolEval b2 w s

    | IsDigit(c) -> Char.IsDigit (charEval c w s)
    | IsLetter(c) -> Char.IsLetter (charEval c w s)
    | IsVowel(c) -> ("aeiouæøåAEIOUÆØÅ ".Contains(charEval c w s))