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

type alias MealtimeMenu =
  {
    mealtimeId: Int,
    days: List DayMenu
  }

type alias DayMenu =
  {
    dayId: Int,
    meals: List Meal
  }

type alias Meal =
  {
    categorieId: Int,
    mealId: Int
  }

type alias Model =
  {
    weekMenu: List List List String,
    mealsDataBase: List AddMealForm.MealsOfCategory
    isOpenAddForm: Bool,
    addForm: AddMealForm.Model
  }

init :  (Model, Cmd Msg)
init =
  let
    (initAddForm, cmdAddForm) = AddMealForm.init
  in
    ( Model [] False initAddForm, Cmd.batch [Cmd.map AddMealFormMsg cmdAddForm, AddMealForm.getMeals] )

-- UPDATE


type Msg
  = AddDish Int String
  | NewWeekMenu (Result Http.Error (List MealtimeMenu))
  | SendDayIdAndMeal (Result Http.Error String)
  | AddMealFormMsg AddMealForm.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddDish dayId mealName ->
      ({model | isOpenAddForm = True}, postDayAndMealId dayId mealName)
    NewWeekMenu (Ok jsonWeekMenu) ->
      let
       weekMenu = idToString jsonWeekMenu model.mealsDataBase
      in
      ({ model | weekMenu = weekMenu}, Cmd.none)
    NewWeekMenu (Err _) ->
      ( model, Cmd.none )
    SendDayIdAndMeal (Ok _) ->
      ( model, Cmd.none)
    SendDayIdAndMeal (Err _) ->
      ( model, Cmd.none )

    AddMealFormMsg action->
      if ( model.isOpenAddForm == False) then
        case action of
          AddMealForm.NewMeals (Ok jsonMeals) ->
            ( {model | mealsDataBase = jsonMeals}, getWeekMenu)
          AddMealForm.NewMeals (Err _) ->
            ( {model | mealsDataBase = []}, Cmd.none)
      else
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
             table[] (List.map (\mealtime -> viewMealtimeMenu mealtime) model.weekMenu)
       ],
       viewAddMealForm model.isOpenAddForm model.addForm
     ]

-- OTHER

idToString: List MealtimeMenu -> List AddMealForm.MealsOfCategory-> List List List String
idToString jsonWeekMenu mealDataBase=
  List.map (\mealtime -> (List.map (\day -> (List.map (\meal ->
    --IDs for ready server only!!! 
    Maybe.withDefault AddMealForm.errorMealsOfCategory List.head
      (List.filter (\category -> (category.id == meal.categoryId)) mealDataBase)
    )) day.meals) mealtime.days)) jsonWeekMenu

viewAddMealForm : Bool -> AddMealForm.Model ->  Html Msg
viewAddMealForm isOpen addForm =
  if (isOpen) then
    div[style <| formStyle "visible"][ Html.map AddMealFormMsg <| AddMealForm.view addForm]
  else
    div[style <| formStyle "hidden"][ Html.map AddMealFormMsg <| AddMealForm.view addForm]


viewMealtimeMenu : MealtimeMenu ->Html Msg
viewMealtimeMenu mealtimeMenu =
       tr[] (List.map (\day ->
        td[style tdStyles][
            viewDayMenu day,
            viewAddDishButton 0 "snack"
        ])
      mealtimeMenu.days)


viewDayMenu : DayMenu -> Html Msg
viewDayMenu dayMenu =
   ul [style ulStyles]
   (List.map (\meal -> li [] [ text getMealName meal]) dayMenu.meals)

getMealName : Meal -> String
getMealName meal =
  List.filter ( = meal.categorieId)

getWeekMenu: Cmd Msg
getWeekMenu =
  Http.send NewWeekMenu <|
    Http.get "http://localhost:3000/NJ_weekMenu" (Json.Decode.list decodeMealtimeMenu)

decodeMealMenu : Json.Decode.Decoder MealMenu
decodeMealMenu =
        map2 MealtimeMenu
          (field "mealtimeId" int)
          (field "days" Json.Decode.list decodeDayMenu)

decodeDayMenu : Json.Decode.Decoder Mealtime
decodeDayMenu =
        map2 Mealtime
          (field "dayId" int)
          (field "meals" Json.Decode.list decodeMeal)

decodeMeal : Json.Decode.Decoder Meal
decodeMeal =
        map2 Meal
          (field "categorieId" int)
          (field "mealId" int)

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
        Http.post "http://localhost:3000/addedDayIdAndMeal" (Http.jsonBody addedMealData) string

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
