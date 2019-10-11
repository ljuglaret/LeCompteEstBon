module Main exposing(..)
import CountDown exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser exposing(..)


main = Browser.sandbox { init = init, update = update, view = view }

type alias Model =
  { x1  : Maybe Int
  , l1  : List Int
  }

init : Model 
init = Model Nothing []

type Msg
  = X  (Result String Int)
  | L (Result String (List Int))


update : Msg -> Model -> Model
update msg {x1, l1 } =
  case msg of
    X (Ok x0) ->
      Model (Just x0)  l1 

    X (Err err ) -> 
        Model Nothing l1 

    L (Ok l0) ->
      Model x1 l0

    L (Err err) ->
      Model x1 []


toInt2 : String -> Result String Int 
toInt2 str = Result.fromMaybe "nope" (String.toInt str)

view : Model -> Html Msg
view model =     div [ ][
    input [ type_ "String", placeholder " Valeur de X  ", onInput ( X<< toInt2) ] []
    , input [ type_ "String", placeholder " Valeur de L ", onInput (L<<toListe) ] []
    ,br[][]
    ,br[][]
    ,case (model.x1) of 
            Just x0  -> text (Debug.toString (solutions model.l1 x0))
            _             -> text (" ")
    ]

pureM : a -> Maybe a 
pureM = Just

etoileM : Maybe (a -> b ) -> Maybe a -> Maybe b 
etoileM g m = g |> Maybe.andThen (\g2 -> 
                 m |> Maybe.andThen (\m2 -> Just (g2 m2)))

sequenceM : List (Maybe a ) -> Maybe (List a )
sequenceM l = case l of 
                [] ->pureM []
                x::xs -> etoileM (etoileM (pureM (::)) x) (sequenceM xs)


toListe  : String -> Result String (List Int)
toListe str = 
    let 
        l = String.split " " str 
        li = List.map(\elt -> String.toInt elt) l 
        lm = sequenceM li
    in Result.fromMaybe "Nope" lm