module AddMealForm exposing (Model, Msg(..), init, update, view)

{-| A customizable AddMealForm component.
This AddMealForm has a dynamic list of items.

# Definition
@docs Model, Msg
# Init
@docs init
# Update
@docs update
# View
@docs view
-}

import Html exposing (Html, div, text, button, input, a)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http  exposing (..)
import Json.Decode exposing (list, string, map6, field, decodeString)
import Json.Encode exposing (list, string)
import Dropdown

-- MODEL
{-| the model. -}
type alias Model =
  { categories : Dropdown.Dropdown,
    meals: Dropdown.Dropdown,
    comment: String,
    input: String
  }

type alias Meal =
  {
    breakfast: List String,
    first: List String,
    second: List String,
    salad: List String,
    snack: List String,
    withTea: List String
  }

{-| init. -}
init : (Model, Cmd Msg)
init =
   ( Model ( Dropdown.init 0 [""] False) (Dropdown.init 1 [""] True) "" "", getCategories)

-- UPDATE

{-| messeges. -}
type Msg = NewCategories (Result Http.Error (List String))
  | NewMeals (Result Http.Error (List String))
  | DropdownMsg Int Dropdown.Msg
  | AddComment
  | Input String
  | AddMeal
  | SendMeal (Result Http.Error (List String))

{-| Update. -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NewCategories (Ok categories) ->
      ( {model | categories = (Dropdown.init model.categories.id categories model.categories.isEditable)}, getMeals "завтрак")

    NewCategories (Err _) ->
      ( {model | categories = (Dropdown.init model.categories.id ["error"] model.categories.isEditable)}, Cmd.none )

    NewMeals (Ok meals) ->
      ( {model | meals = (Dropdown.init model.meals.id meals model.meals.isEditable)}, Cmd.none )

    NewMeals (Err _) ->
      ( {model | meals = (Dropdown.init model.meals.id ["error"] model.meals.isEditable)}, Cmd.none )

    DropdownMsg 0 action->
      case action of
        Dropdown.Select item  ->
          ( {model | categories = Dropdown.update action model.categories},
          if (item == model.categories.selected) then
            Cmd.none
          else
            getMeals item)
        Dropdown.Toogle ->
            ( {model | categories = Dropdown.update action model.categories}, Cmd.none )
        _ ->
          ( model, Cmd.none)

    DropdownMsg 1 action->
      ( {model | meals = Dropdown.update action model.meals}, Cmd.none)

    DropdownMsg _ action->
      ( model, Cmd.none)

    Input comment ->
        ( {model | input = comment}, Cmd.none)
    AddComment ->
        ( {model | comment = model.input}, Cmd.none)

    AddMeal ->
      ( model, postMeal model.meals.selected)

    SendMeal (Ok _) ->
      ( model, Cmd.none)
    SendMeal (Err _) ->
      ( model, Cmd.none )


postMeal: String ->  Cmd Msg
postMeal mealName =
  let
    addedMealName =
        Json.Encode.object
          [ ("addedMeal", Json.Encode.string mealName)]
  in
  Http.send SendMeal<|
      Http.post "http://localhost:3000/addedMeal" (Http.jsonBody addedMealName) (Json.Decode.list Json.Decode.string)

getCategories : Cmd Msg
getCategories =
  Http.send NewCategories <|
    Http.get "http://localhost:3000/mealCategories" (Json.Decode.list Json.Decode.string)

getMeals: String -> Cmd Msg
getMeals categorie =
  Http.send NewMeals <|
    Http.get "http://localhost:3000/meals"  (decodeMeal categorie)

decodeMeal : String -> Json.Decode.Decoder (List String)
decodeMeal categorie =
        field categorie (Json.Decode.list Json.Decode.string)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW
{-| view. -}
view : Model -> Html Msg
view model =
    div [style container][
          div [style divStyles][
            text "Категория:",
             Html.map (DropdownMsg model.categories.id) <| Dropdown.view model.categories
          ],
          div [style divStyles][
            text "Блюдо:",
             Html.map (DropdownMsg model.meals.id) <| Dropdown.view model.meals
          ],
          div [style divStyles][
            div[][text "Комментарий:\n"],
            div[][text (model.comment ++ "\n")],
            input [ type_ "text", placeholder "Input comment", onInput Input] [],
            button [onClick AddComment] [ text "Add comment" ]
          ],
          button [style bigButton, onClick AddMeal ] [ text "Add" ]
    ]

-- OTHER

divStyles : List (String, String)
divStyles =
    [
        ("margin-top", "5px")
    ]

bigButton : List (String, String)
bigButton =
    [
        ("margin-top", "10px"),
        ("width", "100px"),
        ("height", "30px"),
        ("background-color", "green"),
        ("color", "white")
    ]

container : List (String, String)
container =
  [
      ("margin-left", "40px"),
      ("margin-top", "10px"),
      ("padding", "20px"),
      ("border","1px solid black"),
      ("width", "300px")
  ]
