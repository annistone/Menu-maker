module ServerApi exposing(..)

import String
import Json.Decode exposing (list, string, map7, field, int, bool, array)
import List
import Array
import Maybe
import Base64 exposing (..)
import Json.Decode.Extra exposing ((|:))
import Json.Encode exposing (object, string, int, list)
import Http exposing (jsonBody, Body, request, expectStringResponse, Request, emptyBody, expectJson)
import WeekMenu exposing (Meal)
import Secret


type Msg
    = NewWeekMenu (Result Http.Error (Array.Array WeekMenu.DayMenu))
    | NewMealsCatalog (Result Http.Error (Array.Array WeekMenu.Meal))
    | NewComponentsCatalog (Result Http.Error (Array.Array WeekMenu.Component))


deleteWithAuthorization : String -> Request ()
deleteWithAuthorization url =
    let encodedAuthString =
        Result.withDefault ""
        <| Base64.encode Secret.apiKey
    in request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" <| "Basic " ++ encodedAuthString]
        , url = Secret.url ++ url
        , body = emptyBody
        , expect =  expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }



putWithAuthorization : Body -> String -> Request ()
putWithAuthorization menuBody url =
    let encodedAuthString =
        Result.withDefault ""
        <| Base64.encode Secret.apiKey
    in request
        { method = "PUT"
        , headers = [ Http.header "Authorization" <| "Basic " ++ encodedAuthString]
        , url = Secret.url ++ url
        , body = menuBody
        , expect =  expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }

postWithAuthorization : Body -> String -> Request ()
postWithAuthorization menuBody url =
    let encodedAuthString =
        Result.withDefault ""
        <| Base64.encode Secret.apiKey
    in request
        { method = "POST"
        , headers = [ Http.header "Authorization" <| "Basic " ++ encodedAuthString]
        , url = Secret.url ++ url
        , body = menuBody
        , expect =  expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }

getWithAuthorization : String -> Json.Decode.Decoder a -> Request a
getWithAuthorization url decoder =
    let encodedAuthString =
        Result.withDefault ""
        <| Base64.encode Secret.apiKey
    in request
        { method = "GET"
        , headers = [ Http.header "Authorization" <| "Basic " ++ encodedAuthString]
        , url = Secret.url ++ url
        , body = emptyBody
        , expect = expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }


formNewDayMenuBody: Int -> Int -> Int -> Http.Body
formNewDayMenuBody mealtimeId dayId mealId =
    Http.jsonBody
    <| Json.Encode.object
        [ ("day", Json.Encode.string (WeekMenu.dayNumberToDayName dayId))
        , ("dayId", Json.Encode.int dayId)
        , ("breakfastMenuIds", Json.Encode.list <| List.map (\l -> Json.Encode.int l)
            <| if (mealtimeId == 0) then [mealId] else [])
        , ("lunchMenuIds", Json.Encode.list <| List.map (\l -> Json.Encode.int l)
            <| if (mealtimeId == 1) then [mealId] else [])
        , ("dinnerMenuIds", Json.Encode.list <| List.map (\l -> Json.Encode.int l)
            <| if (mealtimeId == 2) then [mealId] else [])
        , ("snacksIds", Json.Encode.list <| List.map (\l -> Json.Encode.int l)
            <| if (mealtimeId == 3) then [mealId] else [])
        ]

formNewMealBody: WeekMenu.Meal -> Http.Body
formNewMealBody newMeal =
    Http.jsonBody
    <| Json.Encode.object
        [ ("name",  Json.Encode.string newMeal.name)
        , ("mealId", Json.Encode.int newMeal.mealId)
        , ("mealCategorieId", Json.Encode.int newMeal.mealCategorieId)
        , ("componentsIds", Json.Encode.array <| Array.map (\l -> Json.Encode.int l) newMeal.componentsIds)
        , ("recept", Json.Encode.string newMeal.recept)
        , ("cost", Json.Encode.int newMeal.cost)
        , ("calories", Json.Encode.int newMeal.calories)
        , ("proteins", Json.Encode.int newMeal.proteins)
        , ("fats", Json.Encode.int newMeal.fats)
        , ("carbohydrates", Json.Encode.int newMeal.carbohydrates)
        ]


formNewComponentBody: WeekMenu.Component -> Http.Body
formNewComponentBody newComponent =
    Http.jsonBody
    <| Json.Encode.object
        [ ("cost", Json.Encode.int newComponent.cost)
        , ("calories", Json.Encode.int newComponent.calories)
        , ("proteins", Json.Encode.int newComponent.proteins)
        , ("fats", Json.Encode.int newComponent.fats)
        , ("carbohydrates", Json.Encode.int newComponent.carbohydrates)
        , ("name",  Json.Encode.string newComponent.name)
        , ("componentId", Json.Encode.int newComponent.componentId)
        ]


getComponentsCatalog: Cmd Msg
getComponentsCatalog =
    Http.send NewComponentsCatalog
    <| getWithAuthorization "/api/componentscatalog"
    <| field "componentscatalogs" <| Json.Decode.array WeekMenu.componentDecoder


getMealsCatalog: Cmd Msg
getMealsCatalog =
    Http.send NewMealsCatalog
    <| getWithAuthorization "/api/mealscatalog"
    <| field "mealscatalogs" <| Json.Decode.array WeekMenu.mealDecoder


getWeekMenu: Cmd Msg
getWeekMenu =
    Http.send NewWeekMenu
    <| getWithAuthorization "/api/thisweekmenu"
    <| field "thisweekmenus"
    <| Json.Decode.array WeekMenu.dayMenuDecoder
