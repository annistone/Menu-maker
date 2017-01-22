import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Http  exposing (..)
import Json.Decode exposing (list, string)
import Dropdown

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
  { menu : Dropdown.Dropdown }



init :  (Model, Cmd Msg)
init =
  ( Model (Dropdown.init [""]), getDish)

-- UPDATE


type Msg = Click | NewDish (Result Http.Error (List String))
  | DropdownMsg Dropdown.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Click ->
      ( model, getDish )

    NewDish (Ok dishes) ->
      ( Model (Dropdown.init dishes), Cmd.none)

    NewDish (Err _) ->
      ( Model (Dropdown.init ["error"]), Cmd.none )

    DropdownMsg action -> ( {model | menu = Dropdown.update action model.menu}, Cmd.none)

getDish : Cmd Msg
getDish =
  Http.send NewDish <|
    Http.get "http://localhost:3000/dishClasses" (list string)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [][
     Html.map DropdownMsg <| Dropdown.view model.menu
  ]
