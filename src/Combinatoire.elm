module Combinatoire exposing (..)

{-
interleave 4 [1,2]
 => [[4,1,2],[1,4,2],[1,2,4]]
-}

interleave : a ->  List a -> List(List a)
interleave x l = 
    case l of 
        [] -> [[x]]
        y::ys -> (x::y::ys)::(List.map ((::)y) (interleave x ys))

{-
subs[4,1,2]
=> [[],[2],[1],[1,2],[4],[4,2],[4,1],[4,1,2]]    
-}

subs :   List a -> List(List a)
subs  l = 
    case l of 
        [] -> [[]]
        y::ys -> (subs ys)++(List.map ((::)y) (subs ys))


{-
perms[4,1,2]
[[4,1,2],[1,4,2],[1,2,4],[4,2,1],[2,4,1],[2,1,4]]
-}

perms : List a -> List (List a ) 
perms l = 
    case l of 
        [] -> [[]]
        y::ys -> List.concatMap (\ll -> interleave y ll ) (perms ys)

{-
toutes les combinaisons possibles d'une suite d'entiers
-}

choices : List a -> List(List a )
choices l =  List.concatMap (\ll -> perms ll) (subs l) 

{-
    split[1,2,3,4]
    [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
-}
split : List a -> List((List a ),(List a ))
split l = 
    case l of 
        []    ->  []
        [x]   ->  [] 
        x::xs ->  ([x], xs)::(List.map
                                (\(ls,rs) -> (x::ls,rs))
                                (split xs)
                            )
        
--factorielle 

factorielle : Int -> Int  
factorielle n = 
    let
        aux acc mul 
            =
                if (mul == 1)
                then acc
                else  aux (acc * mul) (mul - 1 )
    in aux 1 n

-- prod 8..5 => 8*7*6

prod  : Int -> Int -> Int 
prod x y =
    if (x <= y)
    then  List.foldl (*) 1 (List.range x y)
    else prod y x    

type Fract = Fract{num:Int,den:Int}    

pgcd : Int -> Int -> Int  
pgcd x y =
    if (x>=y)
    then 
        if (modBy y x == 0)
        then y
        else 
            pgcd y (modBy y x)
    else pgcd y x 

-- Coefficients binomiaux 
coeffsBin : Int -> Int ->Fract
coeffsBin k n =
    let 
        num1 = prod (k + 1) n
        den1 = factorielle(n-k)
        p = pgcd num1 den1
    in Fract {  num = num1//p,
                den = den1//p
            }  

{-
8!
5! (8-5)!

8*7*6
(8-5)!
-}