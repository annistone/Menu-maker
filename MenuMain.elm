import Html exposing (Html, div, table, tr, text, th, td, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Json.Decode exposing (list, string, map7, field, int, bool, array)
import Json.Encode
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
    }


init :  (Model, Cmd Msg)
init =
    ({ weekMenu =  Array.fromList []
      , mealsCatalog =  Array.fromList []
      , error = "None"}
    , getWeekMenu)


type Msg
    = NewWeekMenu (Result Http.Error (Array.Array WeekMenu.DayMenu))
    | NewMealsCatalog (Result Http.Error (Array.Array WeekMenu.Meal))


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


viewMealtimeMenus: Int -> Model -> Html Msg
viewMealtimeMenus mealtimeNumber model =
    tr[style unitStyles]
    <| List.append
    [ td[style <| List.append headerStyles unitStyles][text <| WeekMenu.mealtimeNumberToMealtimeName mealtimeNumber]
    ]
    <| List.map (\l -> td[style unitStyles][ text
        <| WeekMenu.mealText mealtimeNumber l model.weekMenu model.mealsCatalog]) [0,1,2,3,4,5,6]


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
