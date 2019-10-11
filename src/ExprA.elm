module ExprA exposing(..)


type Bin = Plus 
             | Moins 
             | Fois 
             | Div      
 

type Expr  = OpeBin   { bin  : Bin , exprg : Expr , exprd : Expr }
            | Const     Int

plus :  Expr  ->  Expr  -> Expr 
plus g d = OpeBin   { bin  = Plus , exprg = g  , exprd = d }
 
moins :  Expr  ->  Expr  -> Expr 
moins  g d = OpeBin   { bin  = Moins , exprg = g  , exprd = d }
 

fois :  Expr  ->  Expr  -> Expr 
fois g d = OpeBin   { bin  = Fois , exprg = g  , exprd = d }
 
divi : Expr  ->  Expr  -> Expr 
divi g d = OpeBin   { bin  = Div , exprg = g  , exprd = d }

--(3 + x ) * (x - y)
exprtest : Expr
exprtest =  fois (plus (Const 3 ) (Const 4)) (Const 2)
