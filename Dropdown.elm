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
  , open: Bool
  }

{-| Initializing the model. -}
init : Int -> List String -> Dropdown
init id valuesList =
    Dropdown id valuesList ( Maybe.withDefault "" <| Array.get 0 <| Array.fromList valuesList ) False

-- UPDATE
{-| Different message types the Dropdown can receive. -}
type Msg
  = Select String
  | Toogle

{-| Elm architecture reducer. -}
update : Msg -> Dropdown -> Dropdown
update msg model =
  case msg of
    Select item ->
      { model | selected = item,
                open = False }
    Toogle ->
      { model | open = not model.open }

-- VIEW
{-| Dropdown view. -}
view : Dropdown -> Html Msg
view model =
  if model.open then
    div [ style divStyles]
    [
        ul [ style listStyles]
        ( List.map (\item ->
            li [ onClick <| Select item, style listStyles] [ text item]) model.items)
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
        ("cursor", "default")
    ]
listStyles : List (String, String)
listStyles =
    [
        ("list-style-type","none"),
        ("margin","0"),
        ("padding","0")
    ]
