import Html exposing (Html, div, table, tr, text, th, td, button, label, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Json.Decode exposing (list, string, map7, field, int, bool, array)
import Http exposing (..)
import List
import Array
import Maybe
import Debug
import Base64 exposing (..)
import Json.Decode.Extra exposing ((|:))


import WeekMenu
import EditMealtime exposing(..)
import EditMeal exposing(..)
import EditComponent exposing(..)
import Styles exposing(..)
import ServerApi exposing(..)

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
    , log: String
    , editMealtime: EditMealtime.EditMealtime
    , editMeal: EditMeal.EditMeal
    , editComponent: EditComponent.EditComponent
    , idToDelete: String
    }


init :  (Model, Cmd Msg)
init =
    let
        (editMealInit,editMealCmd) = EditMeal.init
        (editComponentInit,editComponentCmd) = EditComponent.init
    in
    ({ weekMenu =  Array.fromList []
     , mealsCatalog =  Array.fromList []
     , log = "None"
     , editMealtime = EditMealtime.init
     , editMeal = editMealInit
     , editComponent = editComponentInit
     , idToDelete = ""
     }
    , Cmd.batch
        [ Cmd.map EditMealMsg editMealCmd
        , Cmd.map ServerApiMsg ServerApi.getWeekMenu
        , Cmd.map EditComponentMsg editComponentCmd
        ])


type Msg
    = EditMealtimeMsg EditMealtime.Msg
    | EditMealMsg EditMeal.Msg
    | ServerApiMsg ServerApi.Msg
    | EditComponentMsg EditComponent.Msg
    | DeleteDayMenuFromServer (Result Http.Error ())
    | InputIdToDelete String
    | DeleteDayMenu


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ServerApiMsg action ->
            case action of
            NewWeekMenu (Ok newWeekMenu) ->
                ({model | weekMenu = WeekMenu.sortMenus newWeekMenu}, Cmd.map ServerApiMsg ServerApi.getMealsCatalog)

            NewWeekMenu (Err errorDescr) ->
                ({model | log = toString errorDescr}, Cmd.none )

            NewMealsCatalog (Ok newMealCatalog) ->
                ({model | mealsCatalog = newMealCatalog}, Cmd.none)

            NewMealsCatalog (Err errorDescr) ->
                ({model | log = toString errorDescr}, Cmd.none )

            NewComponentsCatalog (Ok newComponentsCatalog) ->
                (model, Cmd.none)

            NewComponentsCatalog (Err errorDescr) ->
                ({model | log = toString errorDescr}, Cmd.none )

        EditMealtimeMsg action ->
            case action of
            PutMealToMenu (Ok answer) ->
                let (editMealtime,cmd) =
                    EditMealtime.update action model.editMealtime
                in
                ({model | editMealtime = editMealtime}
                , Cmd.batch [Cmd.map EditMealtimeMsg cmd, Cmd.map ServerApiMsg ServerApi.getWeekMenu]
                )

            PostDayMenuToServer (Ok answer) ->
                let (editMealtime,cmd) =
                    EditMealtime.update action model.editMealtime
                in
                ({model | editMealtime = editMealtime}
                , Cmd.batch [Cmd.map EditMealtimeMsg cmd, Cmd.map ServerApiMsg ServerApi.getWeekMenu]
                )

            AddMealToMenu mealId ->
                let (editMealtime,cmd) =
                    EditMealtime.update action model.editMealtime
                in
                ({model | editMealtime = editMealtime}
                , Cmd.batch [Cmd.map EditMealtimeMsg cmd, Cmd.map EditMealtimeMsg <|
                    EditMealtime.addMealToMenu editMealtime model.weekMenu]
                )

            _ ->
                let (editMealtime,cmd) =
                    EditMealtime.update action model.editMealtime
                in
                ({model | editMealtime = editMealtime}, Cmd.map EditMealtimeMsg cmd )

        EditMealMsg action ->
            case action of
            AddMealToCatalog  ->
                let (editMeal,cmd) =
                    EditMeal.update action model.editMeal
                in
                ({model | editMeal = editMeal}
                ,  Cmd.batch [Cmd.map EditMealMsg cmd,  Cmd.map ServerApiMsg ServerApi.getMealsCatalog])

            DeleteMealFromCatalog (Ok answer) ->
                    let (editMeal,cmd) =
                        EditMeal.update action model.editMeal
                    in
                    ({model | editMeal = editMeal}
                    ,  Cmd.batch
                        [ Cmd.map EditMealMsg cmd, Cmd.map ServerApiMsg ServerApi.getWeekMenu])

            _ ->
                let (editMeal,cmd) =
                    EditMeal.update action model.editMeal
                in
                ({model | editMeal = editMeal}, Cmd.map EditMealMsg cmd )

        EditComponentMsg action ->
            case action of
            _ ->
                let (editComponent,cmd) =
                    EditComponent.update action model.editComponent
                in
                ({model | editComponent = editComponent}, Cmd.map EditComponentMsg cmd )

        DeleteDayMenuFromServer (Ok answer) ->
            ({model |
                log = toString answer
            }, Cmd.none)

        DeleteDayMenuFromServer (Err errorDescr) ->
            ({model | log = toString errorDescr}, Cmd.none )

        InputIdToDelete idToDelete ->
            ({model | idToDelete = idToDelete}, Cmd.none)

        DeleteDayMenu ->
            (model , deleteDayMenuFromServer model.idToDelete)


view : Model -> Html Msg
view model =
    div[][
        div[style [("padding", "10px"), ("width", "100%"), ("float", "left"),  ("text-align","center")]]
            [ div[ style [ ("margin", "20px 20px")]]
                [text "Привет! Давай ты составишь меню на эту неделю, а я скажу, что нам надо купить! ^___^"]
            , div[ style [ ("display","inline-block")]]
                [ table[style [ ("border", "1px solid black"), ("border-collapse", "collapse"),("float", "left")]]
                    <| List.append
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
                        <| List.map (\l -> Html.map EditMealtimeMsg <| viewMealtimeMenus l model) [0,1,2,3]
    , viewWeekComponents model
        ]]
        , Html.map EditMealtimeMsg <| EditMealtime.view model.editMealtime model.mealsCatalog
        , Html.map EditMealMsg <| EditMeal.view model.editMeal
        , Html.map EditComponentMsg <| EditComponent.view model.editComponent
        --, viewDeleteDayMenu
        --, div[][text model.log]
        ]


viewWeekComponents : Model -> Html Msg
viewWeekComponents model =
    div[ style <| List.append containerStyles [("background-color","#90CAF9")]][
        div[ style headerStyles][ text "Продукты на неделю:"]
        ,viewComponentsList model
        ]


doNothing : a -> List a -> List a
doNothing a list = list


expression: Int -> List Int -> List Int
expression a b =
    if a /= (
        Maybe.withDefault 0
        <| List.head b) then
        (a::b)
    else (doNothing a b)

viewComponentsList: Model -> Html Msg
viewComponentsList model =
    div[]
        <| List.map (\componentId ->
            div[][text <| WeekMenu.componentIdToComponentName model.editComponent.componentsCatalog componentId]
            )
            <| List.foldr expression []
            <| List.sort
            <| List.concat
            <| List.map (\dayId ->
                List.concat
                <| List.map(\mealtimeId ->
                    WeekMenu.getMealtimeComponents mealtimeId dayId model.weekMenu model.mealsCatalog
                )[0,1,2,3]
            ) [0,1,2,3,4,5,6]


viewMealtimeMenus: Int -> Model -> Html EditMealtime.Msg
viewMealtimeMenus mealtimeNumber model =
    tr[style unitStyles]
    <| List.append
    [ td
        [ style <| List.append headerStyles unitStyles
        ]
        [text <| WeekMenu.mealtimeNumberToMealtimeName mealtimeNumber
        ]
    ]
    <| List.map (\dayNumber ->
        td[ style unitStyles]
            <| List.append
             ( List.map (\(mealText,mealId) ->
                 div[style [("margin-bottom", "5px")]]
                    [ label[][ text <| " - " ++ mealText]
                    , button[onClick <| EditMealtime.DeleteMealFromMealtime mealtimeNumber dayNumber mealId model.weekMenu,
                        style <| buttonSmallStyles Red][ text "x"]
                    ])
                <| WeekMenu.mealTextsAndIds mealtimeNumber dayNumber model.weekMenu model.mealsCatalog
                )
                [div[][button[onClick <| EditMealtime.EditMenu mealtimeNumber dayNumber,
                    style <| buttonSmallStyles Blue][ text "+"]
                ]]
        ) [0,1,2,3,4,5,6]

viewDeleteDayMenu : Html Msg
viewDeleteDayMenu =
    div[]
        [  if True then
            div[ style containerStyles]
                [ input [onInput InputIdToDelete][]
                , button [ style buttonStyles, onClick DeleteDayMenu][text "Delete DayMenu"]
                ]
        else div[][]
        ]

deleteDayMenuFromServer: String -> Cmd Msg
deleteDayMenuFromServer dayId =
    Http.send DeleteDayMenuFromServer
    <| deleteWithAuthorization
    ("/api/thisweekmenu/" ++ dayId)



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
