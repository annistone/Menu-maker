module AddMealForm exposing (Model, Msg(..), init, update, view, getMeals, MealsOfCategory, Meal, errorMealsOfCategory)

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
import Json.Decode exposing (list, string, map6, map2, field, decodeString, int)
import Dropdown
import Array
import Maybe

-- MODEL
{-| the model. -}
type alias Model =
  {
    categories : Dropdown.Dropdown,
    meals: Dropdown.Dropdown,
    comment: String,
    input: String
  }

type alias Category =
  {
    id: Int,
    name: String
  }

type alias MealsOfCategory =
  {
    categoryId: Int,
    mealsOfCategorie: List Meal
  }
type alias Meal =
  {
    id: Int,
    name: String
  }

errorMealsOfCategory: MealsOfCategory
errorMealsOfCategory =
  {
    categoryId = 0,
    mealsOfCategorie = [errorMeal]
  }

errorCategory: Category
errorCategory =
  {
    id = 0,
    name = "error"
  }

errorMeal: Meal
errorMeal =
  {
    id = 0,
    name = "error"
  }

{-| init. -}
init : (Model, Cmd Msg)
init =
   ( Model ( Dropdown.init 0 []) (Dropdown.init 1 []) "" "", getCategories)

-- UPDATE

{-| messeges. -}
type Msg = NewCategories (Result Http.Error (List Category))
  | NewMeals (Result Http.Error (List MealsOfCategory))
  | DropdownMsg Int Dropdown.Msg
  | AddComment
  | Input String
  | AddMeal

{-| Update. -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NewCategories (Ok categories) ->
      ( {model | categories = (Dropdown.init model.categories.id categories)}, getMeals)

    NewCategories (Err _) ->
      ( {model | categories = (Dropdown.init model.categories.id [errorCategory])}, Cmd.none )

    NewMeals (Ok jsonMeals) ->
      let
        meals = Maybe.withDefault errorMealsOfCategory <| Array.get  (model.categories.selected.id - 1)  <| Array.fromList jsonMeals
      in
        ( {model | meals = (Dropdown.init model.meals.id meals.mealsOfCategorie)}, Cmd.none )
    NewMeals (Err _) ->
      ( {model | meals = (Dropdown.init model.meals.id [errorMeal])}, Cmd.none )

    DropdownMsg 0 action->
      case action of
        Dropdown.Select item  ->
          ( {model | categories = Dropdown.update action model.categories},
          if ( item.id == model.categories.selected.id) then
            Cmd.none
          else
            getMeals)
        Dropdown.Toogle ->
            ( {model | categories = Dropdown.update action model.categories}, Cmd.none )

    DropdownMsg 1 action->
      ( {model | meals = Dropdown.update action model.meals}, Cmd.none)

    DropdownMsg _ action->
      ( model, Cmd.none)

    Input comment ->
        ( {model | input = comment}, Cmd.none)
    AddComment ->
        ( {model | comment = model.input}, Cmd.none)

    AddMeal ->
      ( model, Cmd.none)

getCategories : Cmd Msg
getCategories =
  Http.send NewCategories <|
    Http.get "http://localhost:3000/NJ_categories" (Json.Decode.list decodeCategorie)

decodeCategorie : Json.Decode.Decoder Category
decodeCategorie = map2 Category
        (field "id" int)
        (field "name" string)

getMeals: Cmd Msg
getMeals =
  Http.send NewMeals <|
    Http.get "http://localhost:3000/NJ_meals"  (Json.Decode.list decodeMeals)

decodeMeals : Json.Decode.Decoder MealsOfCategory
decodeMeals = map2 MealsOfCategory
    (field "id" int)
    (field "meals" (Json.Decode.list decodeMeal))

decodeMeal : Json.Decode.Decoder Meal
decodeMeal = map2 Meal
    (field "id" int)
    (field "name" string)

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
