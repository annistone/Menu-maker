module EditMealtime exposing(..)


import Html exposing (Html, div, table, tr, text, th, td, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Decode exposing (list, string, map7, field, int, bool, array)
import Http exposing (..)
import List
import Array exposing (Array, toList)
import Base64 exposing (..)
import Json.Decode.Extra exposing ((|:))
import WeekMenu exposing(Meal)
import Styles exposing(..)
import ServerApi exposing(..)


type alias EditMealtime =
    { editMenuFlag: Bool
    , editMenuMealtime: Int
    , editMenuDay: Int
    , editMenuMealId: Int
    , log: String
    }


init :  EditMealtime
init =
     { editMenuFlag = False
     , editMenuMealtime = 0
     , editMenuDay = 0
     , editMenuMealId = 0
     , log = "None"
     }

type Msg
    = EditMenu Int Int
    | AddMealToMenu Int
    | PutMealToMenu (Result Http.Error ())
    | DeleteMealFromMealtime Int Int Int (Array.Array WeekMenu.DayMenu)
    | PostDayMenuToServer (Result Http.Error ())
    | Cancel


update : Msg -> EditMealtime -> (EditMealtime, Cmd Msg)
update msg model =
    case msg of

        EditMenu mealtimeNumber dayNumber ->
            ({model |
                editMenuFlag = True
                , editMenuMealtime = mealtimeNumber
                , editMenuDay = dayNumber
            }, Cmd.none )

        AddMealToMenu mealId ->
            ({model | editMenuMealId = mealId}, Cmd.none)

        PutMealToMenu (Ok answer) ->
            ({model |
                log = toString answer
                , editMenuFlag = False
            }, Cmd.none)

        PutMealToMenu (Err errorDescr) ->
            ({model | log = toString errorDescr}, Cmd.none )

        PostDayMenuToServer (Ok answer) ->
            ({model |
                log = toString answer
                , editMenuFlag = False
            }, Cmd.none)

        PostDayMenuToServer (Err errorDescr) ->
            ({model | log = toString errorDescr}, Cmd.none )

        DeleteMealFromMealtime mealtimeNumber dayNumber mealId weekMenu ->
            (model, deleteMealFromMealtime mealtimeNumber dayNumber mealId weekMenu )

        Cancel ->
            ({model | editMenuFlag = False}, Cmd.none )

view : EditMealtime -> Array.Array WeekMenu.Meal -> Html Msg
view model mealsCatalog =
    div[]
        [  if model.editMenuFlag then
            div[ style containerStyles]
                [ div[ style headerStyles][ text <| String.concat
                    [ "Добавить блюдо на "
                    , WeekMenu.mealtimeNumberToMealtimeName model.editMenuMealtime
                    , ", "
                    , WeekMenu.dayNumberToDayName model.editMenuDay
                    ]]
                , viewMealsCatalog mealsCatalog
                , div[style containerNeutralStyles][button [onClick Cancel, style buttonNeutralStyles][text "Отмена"]]
                ]
        else div[][]
        ]

deleteMealFromMealtime: Int -> Int -> Int -> Array.Array WeekMenu.DayMenu -> Cmd Msg
deleteMealFromMealtime mealtimeNumber dayNumber mealId weekMenu =
    Http.send PutMealToMenu
    <| putWithAuthorization
    (WeekMenu.formEditMealBody mealtimeNumber dayNumber mealId weekMenu WeekMenu.Delete)
    ("/api/thisweekmenu/" ++ .id
        (WeekMenu.dayNumToDayMenu dayNumber weekMenu))


addMealToMenu: EditMealtime -> Array.Array WeekMenu.DayMenu -> Cmd Msg
addMealToMenu model weekMenu =
    let
        dayId = .id (WeekMenu.dayNumToDayMenu model.editMenuDay weekMenu)
    in
    case dayId of
        "None" ->
            Http.send PostDayMenuToServer
            <| postWithAuthorization
            (ServerApi.formNewDayMenuBody model.editMenuMealtime model.editMenuDay model.editMenuMealId)
            ("/api/thisweekmenu/")

        _ ->
            Http.send PutMealToMenu
            <| putWithAuthorization
            (WeekMenu.formEditMealBody model.editMenuMealtime model.editMenuDay model.editMenuMealId weekMenu WeekMenu.Add)
            ("/api/thisweekmenu/" ++ dayId)


viewMealsByCategory : Array.Array WeekMenu.Meal -> Int -> List (Html Msg)
viewMealsByCategory mealsCatalog categoryId =
    List.map (\l -> button [onClick <| AddMealToMenu l.mealId, style buttonStyles][text l.name])
    <| List.filter (\l -> l.mealCategorieId == categoryId)
    <| Array.toList mealsCatalog


viewMealsCatalog: Array.Array WeekMenu.Meal -> Html Msg
viewMealsCatalog mealsCatalog =
    div[][ table[]
        <| List.map (\l ->
            let (mealCategorieName, mealCategorieId) = l in
            tr[]
                [ td[style tableHeaderStyles][text mealCategorieName]
                , td[style tableUnitStyles] <| viewMealsByCategory mealsCatalog mealCategorieId
                ]
            ) [("Завтрак", 1), ("Суп", 2), ("Второе", 3), ("Гарнир", 4), ("Салаты", 5), ("Закуски", 6), ("Десерт", 7), ("Напитки", 8)]
        ]
