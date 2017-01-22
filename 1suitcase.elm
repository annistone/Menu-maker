import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Http  exposing (..)
import Json.Decode exposing (list, string, field, map3, map2)
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
  { suitcase : Suitcase }

type alias Suitcase =
  { book: Book,
    snack: String,
    devices: List String
  }

initSuitcase: Suitcase
initSuitcase =
    { book = {name = "", writter = ""}
    , snack = ""
    , devices = []
    }

type alias Book =
  { name: String,
    writter: String
  }

init :  (Model, Cmd Msg)
init =
  ( Model initSuitcase, getSuitcase)


-- UPDATE


type Msg = NewSuitcase (Result Http.Error Suitcase) | Click

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewSuitcase (Ok suitcase) ->
      ( Model suitcase, Cmd.none)

    NewSuitcase (Err _) ->
      ( model, Cmd.none )

    Click ->
      ( model, getSuitcase )


getSuitcase : Cmd Msg
getSuitcase =
  Http.send NewSuitcase <|
    Http.get "http://localhost:3000/suitcase" decodeSuitcase

decodeSuitcase : Json.Decode.Decoder Suitcase
decodeSuitcase =
          map3 Suitcase
          (field "book" decodeBook)
          (field "snack" string)
          (field "devices" (list string))

decodeBook : Json.Decode.Decoder Book
decodeBook =
          map2 Book
          (field "name" string)
          (field "writter" string)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view : Model -> Html Msg
view model =
  div [][
    button [onClick Click ] [ text "+" ],
    div [][
       text ( "I have a book "
            ++ model.suitcase.book.name
            ++ " written by "
            ++ model.suitcase.book.writter
            ++ ", "
            ++ model.suitcase.snack
            ++ " and "
            ++ String.join " " model.suitcase.devices
            ++ " in my suitcase.")
            ]
  ]
