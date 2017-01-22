module Dropdown exposing (Dropdown, Msg(..), init, update, view)

{-| A customizable Dropdown component.
This Dropdown has a dynamic list of items.

# Definition
@docs Dropdown, Msg
# Init
@docs init
# Update
@docs update
# View
@docs view
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import List
import Array

-- DROPDOWN MODEL
{-| The Dropdown model. -}
type alias Dropdown =
  {
    id: Int
  , items: List String
  , selected: String
  , isOpen: Bool
  , isEditable: Bool
  }

{-| Initializing the model. -}
init : Int -> List String -> Bool -> Dropdown
init id valuesList isEditable =
    Dropdown id valuesList ( Maybe.withDefault "default" <| Array.get 0 <| Array.fromList valuesList ) False isEditable

-- UPDATE
{-| Different message types the Dropdown can receive. -}
type Msg
  = Select String
  | Toogle
  | AddNewItem

{-| Elm architecture reducer. -}
update : Msg -> Dropdown -> Dropdown
update msg model =
  case msg of
    Select item ->
      { model | selected = item,
                isOpen = False }
    Toogle ->
      { model | isOpen = not model.isOpen }

    AddNewItem ->
      model
-- VIEW
{-| Dropdown view. -}
view : Dropdown -> Html Msg
view model =
  if model.isOpen then
    div [ style divStyles]
    [
        ul [ style listStyles]
        ( List.map (\item ->
            li [ onClick <| Select item, style ulStyles] [ text item]) model.items),
        if model.isEditable then
          button [onClick AddNewItem] [ text "Добавить" ]
        else
          div[][]
    ]
  else
    div [ style divStyles, onClick Toogle ]
    [
      text model.selected
    ]

-- OTHER
{-| Dropdown HTML creator method. -}
divStyles : List (String, String)
divStyles =
    [
        ("cursor", "default"),
        ("margin-left","10px")
    ]
listStyles : List (String, String)
listStyles =
    [
        ("list-style-type","circle"),
        ("margin","0px"),
        ("margin-left","10px"),
        ("padding","0")
    ]

ulStyles : List (String, String)
ulStyles =
    [
        ("padding","0")
    ]
