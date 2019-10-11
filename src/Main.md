
Des jeux comme **Le compte est bon** ou son équivalent anglais **countdown** ont pour objectif de trouver, 
à partir d une liste d entiers et d un entier cible, une expression construite avec cette liste et qui soit égale à l'entier cible.
Les contraintes sont :
- seules les opérations d'addition, de soustraction, de multiplication  et de division, ainsi que les parenthèses.
- Toutes les valeurs y compris celles intermédiaires doivent être positives.
- L'utilisation de fractions non égales a un entier sont interdites, par exemple 10/5 est autorisée, mais 2/3 ne l'est pas .

# Partie 1 : Mise en place du calcul
## Etape 1 : Pouvoir utiliser des expressions arithmétiques 

Un type similaire mais plus complexe a été évoqué <a href ="http://www.laure-juglaret.fr/index.php/2019/07/29/calcul/"> Ici</a>.

Pour l'usage actuel ce type peut être simplifié 
```elm
type Bin = Plus 
             | Moins 
             | Fois 
             | Div      

type Expr  = OpeBin   { bin  : Bin , exprg : Expr , exprd : Expr }
            | Const     Int
```
## Etape 2 : Générer toutes les   combinaisons possibles d une liste

### 1 : Placer un entier dans une liste sans changer l'ordre des éléments de la liste

```elm
interleave : Int ->  List Int -> List(List Int)
interleave x l = 
    case l of 
        [] -> [[x]]
        y::ys -> (x::y::ys)::(List.map ((::)y) (interleave x ys))

{-
interleave 4 [1,2,3]
    [[4,1,2,3],[1,4,2,3],[1,2,4,3],[1,2,3,4]]
-}
```
### 2 : Déterminer tous les sous-ensembles 
```elm
subs :   List Int -> List(List Int)
subs  l = 
    case l of 
        [] -> [[]]
        y::ys -> (subs ys)++(List.map ((::)y) (subs ys))
{-
subs[1,2,3]
    [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]
-}

```
### 3 : retourner tous les déplacements possibles des éléments d'une liste
```elm
perms : List Int -> List (List Int ) 
perms l = 
    case l of 
        [] -> [[]]
        y::ys -> List.concatMap (\ll -> interleave y ll ) (perms ys)
{-
perms [1,2,3]
        [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
```

### 4 : Appliquer **perms**  sur chaque liste de **subs** pour obtenir toutes les combinaisons possibles des éléments de la liste.

```elm
choices : List Int -> List(List Int )
choices l =  List.concatMap (\ll -> perms ll) (subs l)  
{-
 choices [1,2,3]
[[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
-}
```


## Etape 3 : Générer toutes les   combinaisons possibles  d'expressions à partir de la liste précédente

### 1 : Déterminer toutes les partitions par paires
```elm
split : List Int -> List((List Int ),(List Int ))
split l = 
    case l of 
        []    ->  []
        [x]   ->  [] 
        x::xs ->  ([x], xs):: (List.map (\(ls,rs) -> (x::ls,rs)) (split xs))
{-
split[1,2,3,4]
[([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
-}
```

### 2

```elm  
ops :  List (Expr -> Expr -> Expr)
ops = [plus,moins,fois,divi]     

combine : Expr -> Expr -> List Expr
combine l r = List.map (\o -> o l r)  ops
```

### 3 

```elm
exprs ll =
    case ll of 
        [] -> []
        [x] -> [Const x]
        ns -> List.concatMap (\(ls,rs) -> (List.concatMap (\l -> (List.concatMap (\r -> (List.map (\e -> identity e) (combine l r ))) (exprs rs))) (exprs ls))) (split ns)


```

## Etape 4 : Filtrer parmi les expressions précédentes lesquelles sont égales à la valeur initiale demandée

Le filtre prend en compte les conditions de départ ainsi que d'autres contraintes permettant de prendre en compte la commutativité de l'addition et de la multiplication. En effet, puisque 2+3 = 3+2 on peut inclure comme contraintes que dans une opération (+ ou *) le membre de gauche soit inférieur au membre de droite, ce qui élimine des doublons.   

Par exemple :

```elm
 solutions [5,3,9] 8
--  -> ["(3 + 5)","((9 / 3) + 5)"]
```

Les diverses contraintes sont exprimées de cette manière : 

```elm
valid : Bin -> Int -> Int ->Bool
valid e x0 y0 = 
    case (e,x0,y0) of 
        (Plus,x,y)  -> x<=y
        (Moins,x,y) ->(x > y)
        (Fois,x,y)  -> (x /= 1 && y/=1 && x<=y)
        (Div,x,y)   -> (y/=1 && modBy y x ==0)
```

# Partie 2 : Mise en place de l'affichage 

## Etape 1 : Model et Msg

```elm
type alias Model =
  { x1  : Maybe Int
  , l1  : List Int
  }

type Msg
  = X  (Result String Int)
  | L (Result String (List Int))

```

## Etape 2 : update et view

Il y a deux paramètres **X** et **L** dans le type *Msg*, lesquels vont recevoir une chaîne de caractère.
Pour le premier il faut récupérer l'entier si c'est possible.

```elm
toInt2 : String -> Result String Int 
toInt2 str = Result.fromMaybe "nope" (String.toInt str)
```
Pour le second il faut récupérer la liste d'entiers si c'est possible.
En utilisant les <a href ="http://www.laure-juglaret.fr/index.php/2019/07/28/applicative/"> Applicatives</a> et plus particulièrement **sequence**.

Pour rappel, **sequence** sur *Maybe* transforme une *List Maybe a* en *Maybe List a*.

Utilisation : 

```elm
toListe  : String -> Result String (List Int)
toListe str = 
    let 
        l : List String
        l = String.split " " str 
        li : List (Maybe Int)
        li  = List.map(\elt -> String.toInt elt) l 
        lm : Maybe (List Int)
        lm = sequenceM li
    in Result.fromMaybe "Nope" lm
```



Le code complet est disponible <a href ="https://github.com/ljuglaret/LeCompteEstBon/tree/gh-pages"> Ici</a> et son utilisation se trouve <a href ="https://ljuglaret.github.io/LeCompteEstBon/"> Ici</a>
