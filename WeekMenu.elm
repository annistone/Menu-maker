module WeekMenu exposing(..)

import String
import Json.Decode exposing (list, string, map7, field, int, bool, array)
import List
import Array
import Maybe
import Base64 exposing (..)
import Json.Decode.Extra exposing ((|:))
import Json.Encode exposing (object, string, int)
import Http exposing (jsonBody, Body)


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


sortMenus: Array.Array DayMenu -> Array.Array DayMenu
sortMenus weekMenu =
    Array.fromList
    <|List.sortBy .dayId
    <| Array.toList weekMenu

dayNumberToDayName: Int -> String
dayNumberToDayName dayNumber =
    case dayNumber of
        0 -> "Понедельник"
        1 -> "Вторник"
        2 -> "Среда"
        3 -> "Четверг"
        4 -> "Пятница"
        5 -> "Суббота"
        6 -> "Воскресенье"
        _ -> ""


mealtimeNumberToMealtimeName: Int -> String
mealtimeNumberToMealtimeName mealtimeNumber =
    case mealtimeNumber of
        0 -> "Затрак"
        1 -> "Обед"
        2 -> "Ужин"
        3 -> "Перекус"
        _ -> ""

mealIdToMealName : Array.Array Meal -> Int -> String
mealIdToMealName  mealsCatalog mealId =
    List.foldr (++) ""
    <| List.map .name
    <| List.filter (\l -> l.mealId == mealId)
    <| Array.toList mealsCatalog


mealtimeNumberToMealtimeMenu: Int -> DayMenu -> List Int
mealtimeNumberToMealtimeMenu mealtimeNumber dayMenu =
    case mealtimeNumber of
        0 -> .breakfastMenuIds dayMenu
        1 -> .lunchMenuIds dayMenu
        2 -> .dinnerMenuIds dayMenu
        3 -> .snacksIds dayMenu
        _ -> []

dayNumToDayMenu: Int -> Array.Array DayMenu -> DayMenu
dayNumToDayMenu dayNumber weekMenu = Maybe.withDefault
        { day = ""
        , dayId = 0
        , id = "None"
        , breakfastMenuIds = []
        , lunchMenuIds = []
        , dinnerMenuIds = []
        , snacksIds = []
        }
    <| Array.get dayNumber weekMenu

mealText: Int -> Int -> Array.Array DayMenu -> Array.Array Meal -> String
mealText mealtimeNumber dayNumber weekMenu mealsCatalog =
    List.foldr (++) ""
    <| List.intersperse ", "
    <| List.map (\l -> mealIdToMealName mealsCatalog l)
    <| mealtimeNumberToMealtimeMenu mealtimeNumber
    <| dayNumToDayMenu dayNumber weekMenu


mealDecoder: Json.Decode.Decoder Meal
mealDecoder =
    Json.Decode.succeed Meal
        |: (field "id" Json.Decode.string)
        |: (field "name" Json.Decode.string)
        |: (field "mealId" Json.Decode.int)
        |: (field "mealCategorieId"  Json.Decode.int)
        |: (field "componentsIds" <|Json.Decode.array Json.Decode.int)
        |: (field "recept" Json.Decode.string)
        |: (field "cost" Json.Decode.int)
        |: (field "calories" Json.Decode.int)
        |: (field "proteins" Json.Decode.int)
        |: (field "fats" Json.Decode.int)
        |: (field "carbohydrates" Json.Decode.int)


dayMenuDecoder: Json.Decode.Decoder DayMenu
dayMenuDecoder =
    map7 DayMenu
        (field "id" Json.Decode.string)
        (field "day" Json.Decode.string)
        (field "dayId" Json.Decode.int)
        (field "breakfastMenuIds" (Json.Decode.list Json.Decode.int))
        (field "lunchMenuIds" (Json.Decode.list Json.Decode.int))
        (field "dinnerMenuIds" (Json.Decode.list Json.Decode.int))
        (field "snacksIds" (Json.Decode.list Json.Decode.int))



formAddMealDayMenu: Int -> Int -> Int -> Array.Array DayMenu -> DayMenu
formAddMealDayMenu editMenuMealtime editMenuDay editMenuMealId weekMenu =
    let dayMenu =
        dayNumToDayMenu editMenuDay weekMenu
    in
    case editMenuMealtime of
        0 -> { dayMenu | breakfastMenuIds = editMenuMealId :: dayMenu.breakfastMenuIds}
        1 -> { dayMenu | lunchMenuIds = editMenuMealId :: dayMenu.lunchMenuIds}
        2 -> { dayMenu | dinnerMenuIds = editMenuMealId :: dayMenu.dinnerMenuIds}
        3 -> { dayMenu | snacksIds = editMenuMealId :: dayMenu.snacksIds}
        _ -> dayMenu


formAddMealBody: Int -> Int -> Int -> Array.Array DayMenu -> Http.Body
formAddMealBody editMenuMealtime editMenuDay editMenuMealId weekMenu =
    let dayMenu =
        formAddMealDayMenu editMenuMealtime editMenuDay editMenuMealId weekMenu
    in
    Http.jsonBody
    <| Json.Encode.object
        [ ("day", Json.Encode.string dayMenu.day)
        , ("breakfastMenuIds", Json.Encode.list (List.map (\l -> Json.Encode.int l) dayMenu.breakfastMenuIds))
        , ("lunchMenuIds", Json.Encode.list (List.map (\l -> Json.Encode.int l) dayMenu.lunchMenuIds))
        , ("dinnerMenuIds", Json.Encode.list (List.map (\l -> Json.Encode.int l) dayMenu.dinnerMenuIds))
        , ("snacksIds", Json.Encode.list (List.map (\l -> Json.Encode.int l) dayMenu.snacksIds))
        , ("dayId", Json.Encode.int dayMenu.dayId)
        ]
