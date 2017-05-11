import Html exposing (Html, div, table, tr, text, th, td, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Json.Decode exposing (list, string, map7, field, int, bool, array)
import Http  exposing (..)
import List
import Array
import Maybe
import Debug
import Base64 exposing (..)
import Json.Decode.Extra exposing ((|:))
import WeekMenu


main
    = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
    { weekMenu: Array.Array WeekMenu.DayMenu
    , mealsCatalog: Array.Array WeekMenu.Meal
    , error: String
    , editMenuFlag: Bool
    , editMenuMealtimeAndDay: (Int, Int)
    , editMenuViewMealsFlag: Bool
    }


init :  (Model, Cmd Msg)
init =
    ({ weekMenu =  Array.fromList []
     , mealsCatalog =  Array.fromList []
     , error = "None"
     , editMenuFlag = False
     , editMenuMealtimeAndDay = (0,0)
     , editMenuViewMealsFlag = False
     }
    , getWeekMenu)


type Msg
    = NewWeekMenu (Result Http.Error (Array.Array WeekMenu.DayMenu))
    | NewMealsCatalog (Result Http.Error (Array.Array WeekMenu.Meal))
    | EditMenu Int Int
    | OpenMealsCatalog
    | AddMealToMenu Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewWeekMenu (Ok newWeekMenu) ->
            ({model | weekMenu = WeekMenu.sortMenus newWeekMenu}, getMealsCatalog)

        NewWeekMenu (Err errorDescr) ->
            ({model | error = toString errorDescr}, Cmd.none )

        NewMealsCatalog (Ok newMealCatalog) ->
            ({model | mealsCatalog = newMealCatalog}, Cmd.none)

        NewMealsCatalog (Err errorDescr) ->
            ({model | error = toString errorDescr}, Cmd.none )

        EditMenu mealtimeNumber dayNumber ->
            ({model |
                editMenuFlag = True
                , editMenuMealtimeAndDay = (mealtimeNumber, dayNumber)
            }, Cmd.none )

        OpenMealsCatalog ->
            ({model | editMenuViewMealsFlag = True}, Cmd.none )

        AddMealToMenu mealId ->
            (model, addMealToMenu mealId)


view : Model -> Html Msg
view model =
    div[]
        [ table[style tableStyles] <| List.append
                [ tr[][
                    th[style unitStyles][text ""]
                    , th[style unitStyles][text "Понедельник"]
                    , th[style unitStyles][text "Вторник"]
                    , th[style unitStyles][text "Среда"]
                    , th[style unitStyles][text "Четверг"]
                    , th[style unitStyles][text "Пятница"]
                    , th[style unitStyles][text "Суббота"]
                    , th[style unitStyles][text "Воскресенье"]
                    ]
                ]
                <| List.map (\l -> viewMealtimeMenus l model) [0,1,2,3]

        , if model.editMenuFlag then
            let (mealtimeNumber, dayNumber) = model.editMenuMealtimeAndDay
            in
            div[ style containerStyles]
                [ div[][ text <| String.concat
                    [ "Изменить "
                    , WeekMenu.mealtimeNumberToMealtimeName mealtimeNumber
                    , " в "
                    , WeekMenu.dayNumberToDayName dayNumber
                    ]]
                , button [onClick OpenMealsCatalog, style buttonStyles][text "Добавить блюдо"]
                , if model.editMenuViewMealsFlag then
                    viewMealsCatalog model
                else div[][]
                ]
        else div[][]
        , div[][ text <| "Error:" ++ model.error]
        ]

addMealToMenu: Int -> Cmd Msg
addMealToMenu mealId = Cmd.none

viewMealsByCategory : Model -> Int -> List (Html Msg)
viewMealsByCategory model categoryId =
    List.map (\l -> button [onClick <| AddMealToMenu l.mealId][text l.name])
    <| List.filter (\l -> l.mealCategorieId == categoryId)
    <| Array.toList model.mealsCatalog


viewMealsCatalog: Model -> Html Msg
viewMealsCatalog model =
    div[][ table[style tableStyles]
        <| List.map (\l ->
            let (mealCategorieName, mealCategorieId) =
                l
            in
            tr[]
                [ th[style <| List.append headerStyles unitStyles][text mealCategorieName]
                , th[style unitStyles] <| viewMealsByCategory model mealCategorieId
                ]
            ) [("Завтрак", 1), ("Суп", 2), ("Второе", 3), ("Гарнир", 4), ("Салаты", 5), ("Закуски", 6), ("Десерт", 7), ("Напитки", 8)]
        ]


viewMealtimeMenus: Int -> Model -> Html Msg
viewMealtimeMenus mealtimeNumber model =
    tr[style unitStyles]
    <| List.append
    [ td
        [ style <| List.append headerStyles unitStyles
        ]
        [text <| WeekMenu.mealtimeNumberToMealtimeName mealtimeNumber
        ]
    ]
    <| List.map (\dayNumber -> td
        [ style <| List.append unitStyles pointerStyles
        , onClick <| EditMenu mealtimeNumber dayNumber
        ]
        [ text <| WeekMenu.mealText mealtimeNumber dayNumber model.weekMenu model.mealsCatalog
        ]) [0,1,2,3,4,5,6]


getWithAuthorization : String -> Json.Decode.Decoder a -> Request a
getWithAuthorization url decoder =
    let encodedAuthString =
        Result.withDefault ""
        <| Base64.encode "XfdfydY0q4ha0mOPWcHOli9+Il23vxWy:"
    in request
        { method = "GET"
        , headers = [ Http.header "Authorization" <| "Basic " ++ encodedAuthString]
        , url = url
        , body = emptyBody
        , expect = expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


putWithAuthorization : String -> Request ()
putWithAuthorization url =
    let encodedAuthString =
        Result.withDefault ""
        <| Base64.encode "XfdfydY0q4ha0mOPWcHOli9+Il23vxWy:"
    in request
        { method = "PUT"
        , headers = [ Http.header "Authorization" <| "Basic " ++ encodedAuthString]
        , url = url
        , body = emptyBody
        , expect =  expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


getMealsCatalog: Cmd Msg
getMealsCatalog =
    Http.send NewMealsCatalog
    <| getWithAuthorization "http://localhost:8080/api/mealscatalog"
    <| field "mealscatalogs" <| Json.Decode.array WeekMenu.mealDecoder

getWeekMenu: Cmd Msg
getWeekMenu =
    Http.send NewWeekMenu
    <| getWithAuthorization "http://localhost:8080/api/thisweekmenu"
    <| field "thisweekmenus"
    <| Json.Decode.array WeekMenu.dayMenuDecoder


buttonStyles: List (String, String)
buttonStyles =
    [ ("background-color", "green")
    , ("color", "white")
    , ("margin-top", "20px")
    ]


containerStyles: List (String, String)
containerStyles =
    [ ("border", "1px solid green")
    , ("margin-top", "60px")
    , ("padding", "10px")
    , ("width", "300px")
    ]

tableStyles : List (String, String)
tableStyles =
    [ ("border", "1px solid black")
    , ("margin", "0 auto")
    , ("margin-top", "60px")
    , ("border-collapse", "collapse")
    ]


unitStyles : List (String, String)
unitStyles =
    [ ("border","1px solid black")
    , ("padding", "10px")
    ]

pointerStyles : List (String, String)
pointerStyles =
    [ ("cursor", "pointer")
    ]

headerStyles : List (String, String)
headerStyles =
    [ ("font-weight", "bold")
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
