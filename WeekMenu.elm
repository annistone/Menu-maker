module WeekMenu exposing(..)

import String
import Json.Decode exposing (list, string, map7, field, int, bool, array)
import List
import Array
import Maybe
import Base64 exposing (..)
import Json.Decode.Extra exposing ((|:))


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


mealText: Int -> Int -> Array.Array DayMenu -> Array.Array Meal -> String
mealText mealtimeNumber dayNumber weekMenu mealsCatalog =
    List.foldr (++) ""
    <| List.intersperse ", "
    <| List.map (\l -> mealIdToMealName mealsCatalog l)
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
    <| Array.get dayNumber weekMenu


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
