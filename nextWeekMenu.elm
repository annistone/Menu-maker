import Html exposing (Html, div, table, tr, text, th, td, button, ul, li, a, col, colgroup)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Json.Decode exposing (list, string, map5, field, int)
import Json.Encode
import Http  exposing (..)
import List
import AddMealForm


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias OneDayMenu =
  {
    dayId: Int,
    breakfast: List String,
    lunch: List String,
    dinner: List String,
    snack: List String
  }

type alias Model =
  {
    weekMenu: List OneDayMenu,
    isOpenAddForm: Bool,
    addForm: AddMealForm.Model
  }

init :  (Model, Cmd Msg)
init =
  let
    (initAddForm, cmdAddForm) = AddMealForm.init
  in
    ( Model [] False initAddForm, Cmd.batch [getWeekMenu, Cmd.map AddMealFormMsg cmdAddForm] )

-- UPDATE


type Msg
  = AddDish Int String
  | NewWeekMenu (Result Http.Error (List OneDayMenu))
  | SendDayIdAndMeal (Result Http.Error String)
  | AddMealFormMsg AddMealForm.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddDish dayId mealName ->
      ({model | isOpenAddForm = True}, postDayAndMealId dayId mealName)
    NewWeekMenu (Ok weekMenu) ->
      ({ model | weekMenu = weekMenu}, Cmd.none)
    NewWeekMenu (Err _) ->
      ( model, Cmd.none )
    SendDayIdAndMeal (Ok _) ->
      ( model, Cmd.none)
    SendDayIdAndMeal (Err _) ->
      ( model, Cmd.none )

    AddMealFormMsg action->
      case action of
          AddMealForm.AddMeal ->
              let
                (newAddForm, cmdAddForm) = AddMealForm.update action model.addForm
              in
              ({model | isOpenAddForm = False,
                        addForm = newAddForm}, Cmd.map AddMealFormMsg cmdAddForm) --Server update weekMenu
          _ ->
            let
              (newAddForm, cmdAddForm) = AddMealForm.update action model.addForm
            in
              ( {model | addForm = newAddForm}, Cmd.map AddMealFormMsg cmdAddForm )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

view : Model -> Html Msg
view model =
     div[ style containerStyles][
         table[style tableStyles][
            tr[][
                 th[][text ""]
               , th[][text "Понедельник"]
               , th[][text "Вторник"]
               , th[][text "Среда"]
               , th[][text "Четверг"]
               , th[][text "Пятница"]
               , th[][text "Суббота"]
               , th[][text "Воскресенье"]
            ],
             tr[] <| List.append [th[style tdStyles][text "Завтрак"]] (List.map (\day -> viewOneMealtimeMenu day "завтрак") model.weekMenu)
            , tr[] <| List.append [th[style tdStyles][text "Обед"]] (List.map (\day -> viewOneMealtimeMenu day "обед") model.weekMenu)
            , tr[] <| List.append [th[style tdStyles][text "Ужин"]] (List.map (\day -> viewOneMealtimeMenu day "ужин") model.weekMenu)
            , tr[] <| List.append [th[style tdStyles][text "Перекус"]] (List.map (\day -> viewOneMealtimeMenu day "перекус") model.weekMenu)
       ],
       viewAddMealForm model.isOpenAddForm model.addForm
     ]

-- OTHER
viewAddMealForm : Bool -> AddMealForm.Model ->  Html Msg
viewAddMealForm isOpen addForm =
  if (isOpen) then
    div[style <| formStyle "visible"][ Html.map AddMealFormMsg <| AddMealForm.view addForm]
  else
    div[style <| formStyle "hidden"][ Html.map AddMealFormMsg <| AddMealForm.view addForm]


viewOneMealtimeMenu : OneDayMenu -> String ->Html Msg
viewOneMealtimeMenu day mealtime =
    case mealtime of
      "завтрак" ->
        td[style tdStyles][
          viewOneMealMenu day.breakfast,
          viewAddDishButton day.dayId "breakfast"
        ]
      "обед" ->
        td[style tdStyles][
          viewOneMealMenu day.lunch,
          viewAddDishButton day.dayId "lunch"
        ]
      "ужин" ->
        td[style tdStyles][
          viewOneMealMenu day.dinner,
          viewAddDishButton day.dayId "dinner"
        ]
      "перекус" ->
        td[style tdStyles][
          viewOneMealMenu day.snack,
          viewAddDishButton day.dayId "snack"
        ]
      _ -> td[][]

viewOneMealMenu : List String -> Html Msg
viewOneMealMenu meal =
   ul [style ulStyles]
   (List.map (\l -> li [] [ text l]) meal)

getWeekMenu: Cmd Msg
getWeekMenu =
  Http.send NewWeekMenu <|
    Http.get "http://localhost:3000/weekMenu" (Json.Decode.list dayMenu)

dayMenu : Json.Decode.Decoder OneDayMenu
dayMenu =
        map5 OneDayMenu
          (field "dayId" (int))
          (field "breakfast" (Json.Decode.list string))
          (field "lunch" (Json.Decode.list string))
          (field "dinner" (Json.Decode.list string))
          (field "snack" (Json.Decode.list string))


postDayAndMealId: Int -> String ->  Cmd Msg
postDayAndMealId dayId categorieName =
    Http.send SendDayIdAndMeal<|
      let
        addedMealData =
          Json.Encode.object
            [ ("dayId", Json.Encode.int dayId)
            , ("categorie", Json.Encode.string categorieName)
            ]
      in
        Http.post "http://localhost:3000/addedDayIdAndMealtime" (Http.jsonBody addedMealData) string

viewAddDishButton2 : Int -> String -> Html Msg
viewAddDishButton2 dayId mealName =
      a [href "addMeal.elm"][button [onClick (AddDish dayId mealName)] [ text "+" ]]

viewAddDishButton : Int -> String -> Html Msg
viewAddDishButton dayId mealName =
      button [style buttonStyles, onClick (AddDish dayId mealName)] [ text "+" ]

containerStyles : List (String, String)
containerStyles =
      [
          ("width", "100%"),
          ("height", "100%")
      ]

tableStyles : List (String, String)
tableStyles =
    [
        ("border", "1px solid black"),
        ("margin", "0 auto"),
        ("margin-top", "60px")
    ]

listStyles : List (String, String)
listStyles =
    [
        ("list-style-type","none"),
        ("padding","0"),
        ("border","1px solid black"),
        ("overflow","hidden"  )
    ]
ulStyles : List (String, String)
ulStyles =
    [
        ("margin", "0")
    ]
tdStyles : List (String, String)
tdStyles =
    [
        ("border","1px solid black")
    ]
buttonStyles : List (String, String)
buttonStyles =
    [
        ("float","right"),
        ("width","100%")
    ]

formStyle: String -> List (String, String)
formStyle visibility =
  [
    ("visibility", visibility)
  ]
