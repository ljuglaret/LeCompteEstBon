module CountDown exposing (..)
import ExprA exposing(..)
import Combinatoire exposing (..)


apply : Bin -> Int -> Int ->Int
apply e x y = 
    case e of 
        Plus  -> x + y 
        Moins -> x - y
        Fois  -> x * y 
        Div   -> x//y

{-
Règles :
    => la somme est commutative 
    => toutes les opérations doivent fournir un résultat positif
    => le produit est commutatif, 1 est l'élément neutre du produit
    => la division de deux entiers doit renvoyer un entier
-}

valid : Bin -> Int -> Int ->Bool
valid e x0 y0 = 
    case (e,x0,y0) of 
        (Plus,x,y)  -> x<=y
        (Moins,x,y) ->(x > y)
        (Fois,x,y)  -> (x /= 1 && y/=1 && x<=y)
        (Div,x,y)   -> (y/=1 && modBy y x ==0)

continu : Bin -> Int -> Int -> Maybe Int
continu o a b = 
    if (valid o a b)
    then Just (apply o a b)
    else Nothing 

eval : Expr -> List Int  
eval e =
    case e of   
        Const   n                   -> List.filter (\x-> x > 0) [n]
        OpeBin {bin,exprg,exprd}    -> 
                        List.concatMap (\x ->
                            List.filterMap
                                (\y ->  continu bin x y)
                                (eval exprd))
                            (eval exprg)



ops :  List (Expr -> Expr -> Expr)
ops = [plus,moins,fois,divi]     


combine : Expr -> Expr -> List Expr
combine l r = List.map (\o -> o l r)  ops


-- permet d'obtenir toutes les opérations possibles 
-- entre les entiers d'une liste
-- prend en compte les regles établies au préalable.
exprs : List Int -> List Expr
exprs ll =
    case ll of 
        []  ->  []
        [x] ->  [Const x]
        ns  ->  List.concatMap
                    (\(ls,rs) ->
                        (List.concatMap
                            (\l ->
                                (List.concatMap
                                    (\r ->combine l r) 
                                    (exprs rs)))
                            (exprs ls)))
                    (split ns)
                                 
        
solutions : List Int -> Int -> List String 
solutions ns n = 
    let 
        nsp :   List(List Int )
        nsp =   choices ns
        e   :   List Expr
        e   =   List.concatMap(\x -> exprs x ) nsp 
    in 
        case e of 
            s -> List.map
                    (\elt -> affExpr elt )
                    (List.filter
                        (\t-> eval t == [n])
                        s
                    )



--solutions [4,8,5,9] 40

affExpr : Expr  -> String 
affExpr val = 
        case val of 
                OpeBin   { bin , exprg , exprd }   -> 
                    let 
                        entoure ope =  "("++( (affExpr exprg) ++ ope ++  (affExpr exprd))++")" 
                    in
                        case bin of 
                            Plus   ->   entoure " + "
                            Moins  ->   entoure " - "
                            Fois   ->   entoure " * "
                            Div    ->   entoure " / "
                Const     f   ->   String.fromInt f   
                
                    
