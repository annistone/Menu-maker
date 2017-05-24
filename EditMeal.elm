module EditMeal exposing(..)


import Html exposing (Html, div, table, tr, text, th, td, button, input, textarea, label, fieldset)
import Html.Attributes exposing (style, type_, name)
import Html.Events exposing (onInput, onClick)
import Http exposing (..)
import Array exposing (Array, fromList, toList)
import List exposing (sortBy)
import Json.Decode exposing (list, string, field, int, bool, array)


import WeekMenu exposing(Meal)
import Styles exposing(..)
import ServerApi exposing(..)

type alias EditMeal =
    { createMealFlag: Bool
    , newMeal: WeekMenu.Meal
    , editMealsCatalogFlag: Bool
    , componentsCatalog: Array.Array WeekMenu.Component
    , log: String
    , mealsCatalog: Array.Array WeekMenu.Meal
    }


init :  (EditMeal , Cmd Msg)
init =
    ({ createMealFlag = False
    , newMeal = WeekMenu.initMeal
    , editMealsCatalogFlag = False
    , componentsCatalog = Array.fromList []
    , log = "Ok"
    , mealsCatalog = Array.fromList []
    }, Cmd.map ServerApiMsg ServerApi.getMealsCatalog)


type Msg
    = OpenCreateMealWindow Int
    | NewMealName String
    | NewMealRecept String
    | NewMealCost String
    | NewMealCalories String
    | NewMealProteins String
    | NewMealFats String
    | NewMealCarbohydrates String
    | NewMealCategory Int
    | NewMealComponent Int
    | PostMealToCatalog (Result Http.Error ())
    | AddMealToCatalog
    | ServerApiMsg ServerApi.Msg
    | OpenEditMealsCatalogWindow
    | CancelNewMeal
    | Cancel
    | DeleteMeal Int String
    | DeleteMealFromCatalog (Result Http.Error ())


update : Msg -> EditMeal -> (EditMeal, Cmd Msg)
update msg model =
    let meal = model.newMeal in
    case msg of
        ServerApiMsg action ->
            case action of
            NewMealsCatalog (Ok newMealCatalog) ->
                ({model | mealsCatalog = newMealCatalog}, Cmd.none)

            NewMealsCatalog (Err errorDescr) ->
                ({model | log = toString errorDescr}, Cmd.none )

            NewComponentsCatalog (Ok newComponentsCatalog) ->
                ({model | componentsCatalog = newComponentsCatalog}, Cmd.none )

            NewComponentsCatalog (Err errorDescr) ->
                ({model | log = toString errorDescr}, Cmd.none )

            _-> (model, Cmd.none )

        OpenCreateMealWindow categotyId ->
            let newMeal = { meal | mealCategorieId  = categotyId} in
            ({model | newMeal = newMeal
                , createMealFlag = True
            }, Cmd.map ServerApiMsg ServerApi.getComponentsCatalog)

        NewMealName name ->
            let newMeal = { meal | name  = name} in
            ({model | newMeal = newMeal}, Cmd.none )

        NewMealRecept recept ->
            let newMeal = { meal | recept  = recept} in
            ({model | newMeal = newMeal}, Cmd.none )

        NewMealCost cost ->
           let newMeal = { meal | cost  =
               Result.withDefault 0 (String.toInt cost)}
           in
           ({model | newMeal = newMeal}, Cmd.none )

        NewMealCalories calories ->
           let newMeal = { meal | calories  =
               Result.withDefault 0 (String.toInt calories)}
           in
           ({model | newMeal = newMeal}, Cmd.none )

        NewMealProteins proteins ->
           let newMeal = { meal | proteins  =
               Result.withDefault 0 (String.toInt proteins)}
           in
           ({model | newMeal = newMeal}, Cmd.none )

        NewMealFats fats ->
           let newMeal = { meal | fats  =
               Result.withDefault 0 (String.toInt fats)}
           in
           ({model | newMeal = newMeal}, Cmd.none )

        NewMealCarbohydrates carbohydrates ->
           let newMeal = { meal | carbohydrates  =
               Result.withDefault 0 (String.toInt carbohydrates)}
           in
           ({model | newMeal = newMeal}, Cmd.none )

        NewMealCategory categoryId ->
            let newMeal = { meal | mealCategorieId  = categoryId} in
            ({model | newMeal = newMeal}, Cmd.none )

        NewMealComponent componentId ->
            let
                components = meal.componentsIds
                newComponents = Array.push componentId components
                newMeal = { meal | componentsIds  = newComponents}
            in
            ({model | newMeal = newMeal}, Cmd.none )

        PostMealToCatalog (Ok answer) ->
            ({model |
                log = toString answer
            }, Cmd.map ServerApiMsg ServerApi.getMealsCatalog)

        PostMealToCatalog (Err errorDescr) ->
            ({model | log = toString errorDescr}, Cmd.none )

        AddMealToCatalog  ->
            let newMeal = generateMealId meal model.mealsCatalog in
            ({model
                | newMeal = WeekMenu.initMeal
                , createMealFlag = False
            }, addMealToCatalog {model | newMeal = newMeal})

        CancelNewMeal ->
            ({model
                | newMeal = WeekMenu.initMeal
                , createMealFlag = False
            }, Cmd.none)

        OpenEditMealsCatalogWindow ->
            ({model | editMealsCatalogFlag = True}, Cmd.none)

        Cancel ->
            ({model
                | newMeal = WeekMenu.initMeal
                , createMealFlag = False
                , editMealsCatalogFlag = False
            }, Cmd.none)

        DeleteMeal mealIdInt mealIdStr->
            (model, deleteMealFromCatalog mealIdStr)

        DeleteMealFromCatalog (Ok answer) ->
            ({model |
                log = toString answer
            }, Cmd.map ServerApiMsg ServerApi.getMealsCatalog)

        DeleteMealFromCatalog (Err errorDescr) ->
            ({model | log = toString errorDescr}, Cmd.none )


view : EditMeal -> Html Msg
view model =
    if model.editMealsCatalogFlag then
        div[ style containerStyles][
            div[ style headerStyles][ text "Изменить каталог блюд"]
            , viewMealsCatalog model.mealsCatalog
            , if model.createMealFlag then
                div[ style container2Styles]
                    [ div[ style headerStyles][
                        text <| "Создать блюдо в категории " ++ WeekMenu.categoryIdToCategoryName model.newMeal.mealCategorieId]
                    , table[]
                        [ tr[][ td[style tableHeaderStyles][]
                            , td[][ button [ style buttonStyles, onClick AddMealToCatalog][text "Добавить блюдо"]
                                , button [ style buttonNeutralStyles, onClick CancelNewMeal][text "Отмена"]]]
                        , tr[][ td[style tableHeaderStyles][text "Ингридиенты"]
                            , td[style tableUnitStyles]
                                <| List.map (\component ->
                                     div[ style inlineStyles]
                                        [ input [ type_ "checkbox", onClick <| NewMealComponent component.componentId][]
                                        , text component.name]
                                ) <| List.sortBy .name
                                <| Array.toList model.componentsCatalog
                            ]
                        , tr[][ td[style tableHeaderStyles][text "Название"]
                            , td[][ input [ onInput NewMealName ][]]]
                        , tr[][ td[style tableHeaderStyles][text "Рецепт"]
                            , td[][ textarea [ onInput  NewMealRecept] []]]
                        , tr[][ td[style tableHeaderStyles][text "Суммарная стоимость"]
                            , td[][ input [ onInput NewMealCost] []]]
                        , tr[][ td[style tableHeaderStyles][text "Калории"]
                            , td[][ input [onInput NewMealCalories] []]]
                        , tr[][ td[style tableHeaderStyles][text "Белки"]
                            , td[][ input [onInput NewMealProteins] []]]
                        , tr[][ td[style tableHeaderStyles][text "Жиры"]
                            , td[][ input [onInput NewMealFats] []]]
                        , tr[][ td[style tableHeaderStyles][text "Углеводы"]
                            , td[][ input [onInput NewMealCarbohydrates] []]]
                    ]
                    --, div[][ text model.log]
                    ]
            else
                div[][button [onClick Cancel, style buttonNeutralStyles][text "Отмена"]]
        ]
    else
        div[][ button [onClick OpenEditMealsCatalogWindow, style marginButtonStyles][text "Изменить каталог блюд"]]

viewMealsByCategory : Array.Array WeekMenu.Meal -> Int -> List (Html Msg)
viewMealsByCategory mealsCatalog categoryId =
    List.append
        ( List.map (\l ->
            div[style [("margin-bottom", "5px")]]
                [ label[][text l.name]
                , button[onClick <| DeleteMeal l.mealId l.id,
                    style <| buttonSmallStyles Red][ text "x"]
            ])
            <| List.filter (\l -> l.mealCategorieId == categoryId)
            <| Array.toList mealsCatalog
        )
        [div[][button[onClick <| OpenCreateMealWindow categoryId,
            style <| buttonSmallStyles Blue][ text "+"]
        ]]


viewMealsCatalog: Array.Array WeekMenu.Meal -> Html Msg
viewMealsCatalog mealsCatalog =
    div[style containerNeutral2Styles][
        table[style tableStyles]
            [ tr[]
                <| List.map (\mealCategorieName ->
                     td[ style unitStyles][text mealCategorieName]
                ) ["Завтрак", "Суп", "Второе", "Гарнир", "Салаты", "Закуски", "Десерт", "Напитки"]
            , tr[]
                <| List.map (\mealCategorieId ->
                     td[style unitStyles] <| viewMealsByCategory mealsCatalog mealCategorieId
                ) [1, 2, 3, 4, 5, 6, 7, 8]]
        , div[][text "* после удаления блюда из каталога, удали все пустые ячейки в меню."]
    ]


generateMealId: WeekMenu.Meal -> Array.Array WeekMenu.Meal -> WeekMenu.Meal
generateMealId meal mealCatalog =
    let
        num = 1 + (Array.length
            <| Array.filter (\l -> l.mealCategorieId == meal.mealCategorieId) mealCatalog)
    in
    { meal | mealId = 100 + meal.mealCategorieId*10 + num
    }


deleteMealFromCatalog: String -> Cmd Msg
deleteMealFromCatalog mealId =
    Http.send DeleteMealFromCatalog
    <| deleteWithAuthorization
    ("/api/mealscatalog/" ++ mealId)



addMealToCatalog: EditMeal -> Cmd Msg
addMealToCatalog model =
    Http.send PostMealToCatalog
    <| postWithAuthorization
    (ServerApi.formNewMealBody model.newMeal)
    ("/api/mealscatalog")
