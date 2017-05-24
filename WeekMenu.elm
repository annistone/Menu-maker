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

type DeleteOrAddFlag = Delete | Add

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

type alias Component =
    { id : String
    , cost: Int
    , calories: Int
    , proteins: Int
    , fats: Int
    , carbohydrates: Int
    , name: String
    , componentId: Int
    }

initMeal: Meal
initMeal =
        { id = ""
        , name = ""
        , mealId = 0
        , mealCategorieId = 0
        , componentsIds = Array.fromList []
        , recept = ""
        , cost = 0
        , calories = 0
        , proteins = 0
        , fats = 0
        , carbohydrates = 0
        }

initComponent: Component
initComponent =
        { id = ""
        , name = ""
        , componentId = 0
        , cost = 0
        , calories = 0
        , proteins = 0
        , fats = 0
        , carbohydrates = 0
        }

sortMenus: Array.Array DayMenu -> Array.Array DayMenu
sortMenus weekMenu =
    Array.fromList
    <|List.sortBy .dayId
    <| Array.toList weekMenu


categoryIdToCategoryName: Int -> String
categoryIdToCategoryName number =
    case number of
        1 -> "Завтраки"
        2 -> "Супы"
        3 -> "Второе"
        4 -> "Гарниры"
        5 -> "Салаты"
        6 -> "Закуски"
        7 -> "Десерты"
        8 -> "Напитки"
        _ -> ""


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


componentIdToComponentName : Array.Array Component -> Int -> String
componentIdToComponentName  componentsCatalog componentId =
    List.foldr (++) ""
    <| List.map .name
    <| List.filter (\l -> l.componentId == componentId)
    <| Array.toList componentsCatalog



mealIdToMealName : Array.Array Meal -> Int -> String
mealIdToMealName  mealsCatalog mealId =
    List.foldr (++) ""
    <| List.map .name
    <| List.filter (\l -> l.mealId == mealId)
    <| Array.toList mealsCatalog


mealIdToMealComponents : Array.Array Meal -> Int -> List Int
mealIdToMealComponents  mealsCatalog mealId =
    List.concat
    <| List.map Array.toList
    <| List.map .componentsIds
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

mealTextsAndIds: Int -> Int -> Array.Array DayMenu -> Array.Array Meal -> List (String, Int)
mealTextsAndIds mealtimeNumber dayNumber weekMenu mealsCatalog =
     List.map (\l -> (mealIdToMealName mealsCatalog l, l))
    <| mealtimeNumberToMealtimeMenu mealtimeNumber
    <| dayNumToDayMenu dayNumber weekMenu


getMealtimeComponents: Int -> Int -> Array.Array DayMenu -> Array.Array Meal -> List Int
getMealtimeComponents mealtimeNumber dayNumber weekMenu mealsCatalog =
    List.concat
    <| List.map (\l -> mealIdToMealComponents mealsCatalog l)
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


componentDecoder: Json.Decode.Decoder Component
componentDecoder =
    Json.Decode.succeed Component
        |: (field "id" Json.Decode.string)
        |: (field "cost" Json.Decode.int)
        |: (field "calories" Json.Decode.int)
        |: (field "proteins" Json.Decode.int)
        |: (field "fats" Json.Decode.int)
        |: (field "carbohydrates" Json.Decode.int)
        |: (field "name" Json.Decode.string)
        |: (field "componentId" Json.Decode.int)

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



formEditMealDayMenu: Int -> Int -> Int -> Array.Array DayMenu -> DeleteOrAddFlag -> DayMenu
formEditMealDayMenu editMenuMealtime editMenuDay editMenuMealId weekMenu deleteOrAddFlag =
    let dayMenu =
        dayNumToDayMenu editMenuDay weekMenu
    in
    case deleteOrAddFlag of
        Delete ->
            case editMenuMealtime of
                0 -> { dayMenu | breakfastMenuIds = List.filter (\l -> l /= editMenuMealId) dayMenu.breakfastMenuIds}
                1 -> { dayMenu | lunchMenuIds = List.filter (\l -> l /= editMenuMealId) dayMenu.lunchMenuIds}
                2 -> { dayMenu | dinnerMenuIds = List.filter (\l -> l /= editMenuMealId) dayMenu.dinnerMenuIds}
                3 -> { dayMenu | snacksIds = List.filter (\l -> l /= editMenuMealId) dayMenu.snacksIds}
                _ -> dayMenu

        Add ->
            case editMenuMealtime of
                0 -> { dayMenu | breakfastMenuIds = editMenuMealId :: dayMenu.breakfastMenuIds}
                1 -> { dayMenu | lunchMenuIds = editMenuMealId :: dayMenu.lunchMenuIds}
                2 -> { dayMenu | dinnerMenuIds = editMenuMealId :: dayMenu.dinnerMenuIds}
                3 -> { dayMenu | snacksIds = editMenuMealId :: dayMenu.snacksIds}
                _ -> dayMenu


formEditMealBody: Int -> Int -> Int -> Array.Array DayMenu -> DeleteOrAddFlag -> Http.Body
formEditMealBody editMenuMealtime editMenuDay editMenuMealId weekMenu deleteOrAddFlag=
    let dayMenu =
        formEditMealDayMenu editMenuMealtime editMenuDay editMenuMealId weekMenu deleteOrAddFlag
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
