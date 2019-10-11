module CountDown2 exposing (..)
import Combinatoire exposing (..)


type Expr = Val Int
            |App Op Expr Expr

type Op = 
    Add
    |Sub  
    |Mul 
    |Div2 



apply2 : Op -> Int -> Int ->Int
apply2 e x y = 
    case e of 
        Add     ->  x + y 
        Sub     ->  x - y
        Mul     ->  x * y 
        Div2    ->  x//y


valid2 : Op -> Int -> Int ->Bool
valid2 e x0 y0 = 
    case (e,x0,y0) of 
        (Add,x,y)   ->  x<=y
        (Sub,x,y)   ->  (x > y)
        (Mul,x,y)   ->  (x /= 1 && y/=1 && x<=y)
        (Div2,x,y)  ->  (y/=1 && modBy y x ==0)

ops2 : List Op 
ops2 = [Add,Sub,Mul,Div2]


type alias Result = (Expr, Int )
r1 : Result 
r1 = (App Add (Val 2) (Val 3),5)
r2 = (App Sub (Val 21) (Val 6),15)


{-
combine2 r2 r1 == 
    [(App Sub (App Sub (Val 21) (Val 6)) (App Add (Val 2) (Val 3)),10),
        (App Div2 (App Sub (Val 21) (Val 6)) (App Add (Val 2) (Val 3)),3)]

combine2 r1 r2 ==
    [(App Add (App Add (Val 2) (Val 3)) (App Sub (Val 21) (Val 6)),20),
    (App Mul (App Add (Val 2) (Val 3)) (App Sub (Val 21) (Val 6)),75)]

-}
combine2 : Result -> Result -> List Result        
combine2 (l,x)(r,y) = List.map
                        (\o -> (App o l r, apply2 o x y))
                        (List.filter (\elt -> valid2 elt x y ) ops2)


{-
C'est l'équivalent de la fonction exprs mais le résultat final 
est plus réduit grâce à *combine2*
par exemple la taille de 
    exprs [4,5,6,7] est égale à 320
    tandis que la taille de 
    results [4,5,6,7] est égale à 23
Le but est de calculer toutes les expressions possibles à partir d'une
liste d'éléments en
    conservant l'ordre
    respectant les règles établies sur les opérateurs.
-}
results : List Int -> List (Expr,Int)
results li = 
    case li of 
        [] ->   [] 
        [n] ->  List.filter 
                    (\(p,p2) -> p2 > 0)
                    [(Val n , n)]
        ns -> List.concatMap
                        (\(ls,rs) ->
                            (List.concatMap
                                (\l ->
                                    (List.concatMap
                                        (\r ->
                                            combine2 l r) 
                                        (results rs)))
                                (results ls)))
                        (split ns)
            

{-
split[4,5]
    [([4],[5])]
results[4]
    [(Val 4,4)]
results[5]
    [(Val 5,5)] 
combine2 (Val 4,4) (Val 5,5)
    [(App Add (Val 4) (Val 5),9),
    (App Mul (Val 4) (Val 5),20)
    ]
-}

solution2 : List Int -> Int -> List String
solution2 ns n = List.map (\(t1,t2) -> affExpr t1)
                    (List.concatMap
                            (\nsp ->
                                (List.filter
                                    (\(e,m) ->    (m==n) )
                                    (results nsp)))
                            (choices ns))
                            
                                     


                  


affExpr : Expr  -> String 
affExpr val = 
        case val of 
                App  bin  exprg  exprd   -> 
                    let 
                        entoure ope =  "("++( (affExpr exprg) ++ ope ++  (affExpr exprd))++")" 
                    in
                        case bin of 
                            Add   ->   entoure " + "
                            Sub  ->   entoure " - "
                            Mul  ->   entoure " * "
                            Div2    ->   entoure " / "
                Val     f   ->   String.fromInt f   
                
  
exp = solution2 [4,8,5,9] 40


etoileL : List (a -> b ) -> List a -> List b 
etoileL f l = 
    List.concatMap (\g -> List.map g l) f



pureL : a -> List a 
pureL x = [x]


lmap1 : (a -> b) -> List a -> List b
lmap1 g l = etoileL (pureL g) l

lmap2 : (a-> b -> c) -> List a -> List b -> List c  
lmap2 g l1 l2 = etoileL (lmap1 g l1) l2

 
results2 ns   = 
    lmap2
        (\(g,d) -> combine2 (results2 g) (results2 d) )
        (Tuple.first(List.unzip (split ns)))
        (Tuple.second(List.unzip (split ns)))