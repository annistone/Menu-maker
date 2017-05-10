import Html exposing (Html, div, table, tr, text, th, td, button, ul, li, a, col, colgroup)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Json.Decode exposing (list, string, map4, map7, field, int, bool, array)
import Json.Encode
import Http  exposing (..)
import List
import Array
import Maybe
import Debug
import Base64 exposing (..)
import Json.Decode.Extra exposing ((|:))


main
    = Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
    { weekMenu: Array.Array DayMenu
    , mealsCatalog: Array.Array Meal
    , error: String
    }


type alias Meal =
    { id : String
    , name: String
    , mealId: Int
    , mealCategorieId : Int
    , componentsIds: Array.Array Int
    , recept: String
    , cost: Int
    , calories: Int
    , proteins: Int
    , fats: Int
    , carbohydrates: Int
    }


type alias DayMenu =
    { id: String
    , day: String
    , dayId: Int
    , breakfastMenuIds: List Int
    , lunchMenuIds: List Int
    , dinnerMenuIds: List Int
    , snacksIds: List Int
    }


init :  (Model, Cmd Msg)
init =
    ({ weekMenu =  Array.fromList []
      , mealsCatalog =  Array.fromList []
      , error = "None"}
    , getWeekMenu)


type Msg
    = NewWeekMenu (Result Http.Error (Array.Array DayMenu))
    | NewMealsCatalog (Result Http.Error (Array.Array Meal))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NewWeekMenu (Ok newWeekMenu) ->
            ({model | weekMenu = sortMenus newWeekMenu}, getMealsCatalog)

        NewWeekMenu (Err errorDescr) ->
            ({model | error = toString errorDescr}, Cmd.none )

        NewMealsCatalog (Ok newMealCatalog) ->
            ({model | mealsCatalog = newMealCatalog}, Cmd.none)

        NewMealsCatalog (Err errorDescr) ->
            ({model | error = toString errorDescr}, Cmd.none )


view : Model -> Html Msg
view model =
    div[]
        [ table[style tableStyles]
            <| List.append
            [ tr[]
                [ th[style unitStyles][text ""]
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
        , div[]
            [ text
            <| "Error:" ++ model.error
            ]
        ]


sortMenus: Array.Array DayMenu -> Array.Array DayMenu
sortMenus weekMenu =
    Array.fromList
    <|List.sortBy .dayId
    <| Array.toList weekMenu


mealtimeNumberToMealtimeName: Int -> String
mealtimeNumberToMealtimeName mealtimeNumber =
    case mealtimeNumber of
        0 -> "Затрак"
        1 -> "Обед"
        2 -> "Ужин"
        3 -> "Перекус"
        _ -> ""

viewMealtimeMenus: Int -> Model -> Html Msg
viewMealtimeMenus mealtimeNumber model =
    tr[style unitStyles]
    <| List.append
    [ td[style <| List.append headerStyles unitStyles][text <| mealtimeNumberToMealtimeName mealtimeNumber]
    ]
    <| List.map (\l -> td[style unitStyles][ text <| mealText mealtimeNumber l model]) [0,1,2,3,4,5,6]


mealIdToMealName : Model -> Int -> String
mealIdToMealName model mealId =
    List.foldr (++) ""
    <| List.map .name
    <| List.filter (\l -> l.mealId == mealId)
    <| Array.toList model.mealsCatalog


mealtimeNumberToMealtimeMenu: Int -> DayMenu -> List Int
mealtimeNumberToMealtimeMenu mealtimeNumber dayMenu =
    case mealtimeNumber of
        0 -> .breakfastMenuIds dayMenu
        1 -> .lunchMenuIds dayMenu
        2 -> .dinnerMenuIds dayMenu
        3 -> .snacksIds dayMenu
        _ -> []


mealText: Int -> Int -> Model -> String
mealText mealtimeNumber dayNumber model =
    List.foldr (++) ""
    <| List.intersperse ", "
    <| List.map (\l -> mealIdToMealName model l)
    <| mealtimeNumberToMealtimeMenu mealtimeNumber
    <| Maybe.withDefault
        { day = ""
        , dayId = 0
        , id = "None"
        , breakfastMenuIds = []
        , lunchMenuIds = []
        , dinnerMenuIds = []
        , snacksIds = []
        }
    <| Array.get dayNumber model.weekMenu


getMealsCatalog: Cmd Msg
getMealsCatalog =
    Http.send NewMealsCatalog
    <| getWithAuthorization "http://localhost:8080/api/mealscatalog"
    <| field "mealscatalogs" <| Json.Decode.array mealDecoder

mealDecoder: Json.Decode.Decoder Meal
mealDecoder =
    Json.Decode.succeed Meal
        |: (field "id" string)
        |: (field "name" string)
        |: (field "mealId" int)
        |: (field "mealCategorieId"  int)
        |: (field "componentsIds" <|array int)
        |: (field "recept" string)
        |: (field "cost" int)
        |: (field "calories" int)
        |: (field "proteins" int)
        |: (field "fats" int)
        |: (field "carbohydrates" int)


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


getWeekMenu: Cmd Msg
getWeekMenu =
    Http.send NewWeekMenu
    <| getWithAuthorization "http://localhost:8080/api/thisweekmenu"
    <| field "thisweekmenus"
    <| Json.Decode.array dayMenuDecoder

dayMenuDecoder: Json.Decode.Decoder DayMenu
dayMenuDecoder =
    map7 DayMenu
        (field "id" string)
        (field "day" string)
        (field "dayId" int)
        (field "breakfastMenuIds" (Json.Decode.list int))
        (field "lunchMenuIds" (Json.Decode.list int))
        (field "dinnerMenuIds" (Json.Decode.list int))
        (field "snacksIds" (Json.Decode.list int))


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

headerStyles : List (String, String)
headerStyles =
    [ ("font-weight", "bold")
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
