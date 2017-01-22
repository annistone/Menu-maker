import Html exposing (Html, div, text, tr, td, table)
import Html.Attributes exposing (..)
import String
import List

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model = List String

init :  (Model, Cmd Msg)
init =
  ( ["brick 1", "brick 2", "brick 3", "brick 4", "brick 5"], Cmd.none)


-- UPDATE


type Msg = Click

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    Click ->
      ( model, Cmd.none )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ table[style tableStyles][
        tr[]
         (List.map (\brick -> td[][text brick]) model)
      ]
    ]

-- OTHER

tableStyles : List (String, String)
tableStyles =
    [
        ("border", "1px solid black")
      , ("position", "fixed")
      , ("top", "20%")
      , ("left", "20%")
    ]
