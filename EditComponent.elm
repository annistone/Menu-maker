module EditComponent exposing(..)


import Html exposing (Html, div, table, tr, text, th, td, button, input, textarea, label, fieldset)
import Html.Attributes exposing (style, type_, name)
import Html.Events exposing (onInput, onClick)
import Http exposing (..)
import Array exposing (Array, fromList, toList)
import List exposing (sortBy)
import Json.Decode exposing (list, string, field, int, bool, array)


import WeekMenu exposing(Component)
import Styles exposing(..)
import ServerApi exposing(..)

type alias EditComponent =
    { createComponentFlag: Bool
    , editComponentsCatalogFlag: Bool
    , newComponent: WeekMenu.Component
    , componentsCatalog: Array.Array WeekMenu.Component
    , log: String
    }


init :  (EditComponent , Cmd Msg)
init =
    ({ createComponentFlag = False
    , editComponentsCatalogFlag = False
    , newComponent = WeekMenu.initComponent
    , componentsCatalog = Array.fromList []
    , log = "Ok"
    }, Cmd.map ServerApiMsg ServerApi.getComponentsCatalog)


type Msg
    = OpenCreateComponentWindow
    | NewComponentName String
    | NewComponentCost String
    | NewComponentCalories String
    | NewComponentProteins String
    | NewComponentFats String
    | NewComponentCarbohydrates String
    | PostComponentToCatalog (Result Http.Error ())
    | AddComponentToCatalog
    | ServerApiMsg ServerApi.Msg
    | OpenEditComponentsCatalogWindow
    | DeleteComponentFromCatalog String
    | CancelNewComponent
    | Cancel
    | DeleteComponentFromCatalogServer (Result Http.Error ())


update : Msg -> EditComponent -> (EditComponent, Cmd Msg)
update msg model =
    let component = model.newComponent in
    case msg of
        ServerApiMsg action ->
            case action of
            NewComponentsCatalog (Ok newComponentsCatalog) ->
                ({model | componentsCatalog = newComponentsCatalog}, Cmd.none )

            NewComponentsCatalog (Err errorDescr) ->
                ({model | log = toString errorDescr}, Cmd.none )

            _-> (model, Cmd.none )

        OpenCreateComponentWindow ->
            ({model | createComponentFlag = True}, Cmd.map ServerApiMsg ServerApi.getComponentsCatalog)

        NewComponentName name ->
            let newComponent = { component | name  = name} in
            ({model | newComponent = newComponent}, Cmd.none )

        NewComponentCost cost ->
           let newComponent = { component | cost  =
               Result.withDefault 0 (String.toInt cost)}
           in
           ({model | newComponent = newComponent}, Cmd.none )

        NewComponentCalories calories ->
           let newComponent = { component | calories  =
               Result.withDefault 0 (String.toInt calories)}
           in
           ({model | newComponent = newComponent}, Cmd.none )

        NewComponentProteins proteins ->
           let newComponent = { component | proteins  =
               Result.withDefault 0 (String.toInt proteins)}
           in
           ({model | newComponent = newComponent}, Cmd.none )

        NewComponentFats fats ->
           let newComponent = { component | fats  =
               Result.withDefault 0 (String.toInt fats)}
           in
           ({model | newComponent = newComponent}, Cmd.none )

        NewComponentCarbohydrates carbohydrates ->
           let newComponent = { component | carbohydrates  =
               Result.withDefault 0 (String.toInt carbohydrates)}
           in
           ({model | newComponent = newComponent}, Cmd.none )

        PostComponentToCatalog (Ok answer) ->
            ({model |
                log = toString answer
            }, Cmd.map ServerApiMsg ServerApi.getComponentsCatalog)

        PostComponentToCatalog (Err errorDescr) ->
            ({model | log = toString errorDescr}, Cmd.none )

        AddComponentToCatalog  ->
            let newComponent = generateComponentId component model.componentsCatalog in
            ({model
                | newComponent = newComponent
                , createComponentFlag = False
            }, addComponentToCatalog {model | newComponent = newComponent})

        OpenEditComponentsCatalogWindow ->
            ({model | editComponentsCatalogFlag = True}, Cmd.none )

        DeleteComponentFromCatalog componentId ->
            (model, deleteComponentFromCatalog componentId )

        CancelNewComponent ->
            ({model
                | newComponent = WeekMenu.initComponent
                , createComponentFlag = False
            }, Cmd.none)

        Cancel ->
            ({model
                | newComponent = WeekMenu.initComponent
                , createComponentFlag = False
                , editComponentsCatalogFlag = False
            }, Cmd.none)

        DeleteComponentFromCatalogServer (Ok answer) ->
           ({model |
                log = toString answer
            }, Cmd.map ServerApiMsg ServerApi.getComponentsCatalog)

        DeleteComponentFromCatalogServer (Err errorDescr) ->
            ({model | log = toString errorDescr}, Cmd.none )



view : EditComponent -> Html Msg
view model =
    if model.editComponentsCatalogFlag then
        div[ style containerStyles][
            div[ style headerStyles][ text "Изменить каталог ингридиентов"]
            , viewComponentsCatalog model.componentsCatalog
            , if model.createComponentFlag then
                div[ style container2Styles]
                    [ div[ style headerStyles][ text "Создать ингредиент"]
                    , table[]
                        [ tr[][ td[][]
                            , td[][ button [ style buttonStyles, onClick AddComponentToCatalog][text "Добавить ингридиент"]
                                , button [ style buttonNeutralStyles, onClick CancelNewComponent][text "Отмена"] ]]
                        , tr[][ td[][text "Название"]
                            , td[][ input [ onInput NewComponentName ][]]]
                        , tr[][ td[][text "Суммарная стоимость"]
                            , td[][ input [ onInput NewComponentCost] []]]
                        , tr[][ td[][text "Калории"]
                            , td[][ input [onInput NewComponentCalories] []]]
                        , tr[][ td[][text "Белки"]
                            , td[][ input [onInput NewComponentProteins] []]]
                        , tr[][ td[][text "Жиры"]
                            , td[][ input [onInput NewComponentFats] []]]
                        , tr[][ td[][text "Углеводы"]
                            , td[][ input [onInput NewComponentCarbohydrates] []]]
                    ]
                    ]
            else
                div[style containerNeutralStyles][button [onClick Cancel, style buttonNeutralStyles][text "Отмена"]]
            ]
        else
            div[][ button [onClick OpenEditComponentsCatalogWindow, style marginButtonStyles][text "Изменить каталог ингридиентов"]]


viewComponentsCatalog : Array.Array WeekMenu.Component -> Html Msg
viewComponentsCatalog componentsCatalog =
    div[style containerNeutral2Styles]
        <| List.append
            ( List.map (\l ->
                div[style [("margin-bottom", "5px")]]
                    [ label[][text l.name]
                ])
                <| Array.toList componentsCatalog
            )
            [div[][ button[onClick <| OpenCreateComponentWindow,
                style <| buttonSmallStyles Blue][ text "+"]]
            ]


generateComponentId: WeekMenu.Component -> Array.Array WeekMenu.Component -> WeekMenu.Component
generateComponentId component componentCatalog =
    let
        num = 1 + (Array.length componentCatalog)
    in
    { component | componentId = 20 + num
    }


deleteComponentFromCatalog: String -> Cmd Msg
deleteComponentFromCatalog componentId =
    Http.send DeleteComponentFromCatalogServer
    <| deleteWithAuthorization
    ("/api/componentscatalog/" ++ componentId)


addComponentToCatalog: EditComponent -> Cmd Msg
addComponentToCatalog model =
    Http.send PostComponentToCatalog
    <| postWithAuthorization
    (ServerApi.formNewComponentBody model.newComponent)
    ("/api/componentscatalog")
