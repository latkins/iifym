module IIFYM where

import Html.Shorthand (..)
import Html (..)
import Html
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy, lazy2)
import Json.Decode as Json
import Debug
import List
import Maybe
import Signal
import String
import Window
import Dict

---- MODEL ----
type alias Model =
    { info   : Info
    , macros : Macros
    }

type alias Info =
    { gender       : Gender
    , age          : Int
    , height       : Float 
    , weight       : Float
    , fatPerc      : Float
    , intensity    : Intensity
    , goal         : Goal
    , bmrEquation  : BMREquation
    , macroFormula : MacroFormula
    , units        : Units
    }

type Units = Imperial | Metric

type alias Macros =
    { protein : Float
    , carbs   : Float
    , fat     : Float
    } 

type Action = NoOp
            | UpdateGender Gender
            | UpdateAge Int
            | UpdateHeight Float
            | UpdateWeight Float
            | UpdateFatPerc Float
            | UpdateIntensity Intensity
            | UpdateBMREquation BMREquation
            | UpdateMacroFormula MacroFormula
            | UpdateGoal Goal
            | UpdateUnits Units

type Gender = Male | Female

type BMREquation = MiffinStJeor
                 | KatchMcArdle

-- Intensity should somehow reflect a combo of frequency and intensity.
type Intensity = No
               | Three
               | Four
               | Five
               | Six
               | FiveIntense
               | EveryDay
               | EveryDayIntense
               | TwiceDaily
               | DailyPhysicalJob 

type Goal = CutHard
          | Cut 
          | Maintain
          | Bulk
          | BulkHard

type MacroFormula = TextBook
                  | ZoneDiet
                  | LowFat

---- UPDATE ----
-- Looks like there might be a load of excess calculation here.
-- Probably doesn't matter.
update : Action -> Model -> Model
update action {info, macros} =
    let info' = case action of
                  NoOp -> info
                  UpdateGender gender ->
                      { info | gender <- gender }
                  UpdateAge age ->
                      { info | age <- age }
                  UpdateHeight height ->
                      { info | height <- height }
                  UpdateWeight weight ->
                      { info | weight <- weight }
                  UpdateFatPerc fatPerc ->
                      { info | fatPerc <- fatPerc }
                  UpdateIntensity intensity ->
                      { info | intensity <- intensity }
                  UpdateBMREquation bmrEquation ->
                      { info | bmrEquation <- bmrEquation }
                  UpdateMacroFormula macroFormula ->
                      { info | macroFormula <- macroFormula }
                  UpdateGoal goal ->
                      { info | goal <- goal }
    in { info = info'
       , macros = calcMacros info'
       } 

calcMacros : Info -> Macros
calcMacros info =
    let bmr = calcBMR info
        tdee = calcTDEE bmr info
        goal = calcGoal tdee info
    in
      calcMacros' goal info

calcMacros' : Float -> Info -> Macros
calcMacros' goal {macroFormula} =
    case macroFormula of
      TextBook -> { protein = 0.25 * goal
                  , fat = 0.25 * goal
                  , carbs = 0.50 * goal
                  } 
      ZoneDiet -> { protein = 0.3 * goal
                  , fat = 0.3 * goal
                  , carbs = 0.4 * goal
                  } 
      LowFat -> { protein = 0.25 * goal 
                , fat = 0.15 * goal
                , carbs = 0.6 * goal
                } 

calcGoal : Float -> Info -> Float
calcGoal tdee {goal} =
    case goal of
      CutHard  -> tdee * 0.8
      Cut      -> tdee * 0.85
      Maintain -> tdee * 1.0
      Bulk     -> tdee * 1.05
      BulkHard -> tdee * 1.1

-- Calculate total daily energy expenditure, given a bmr and info.
calcTDEE : Int -> Info -> Float 
calcTDEE bmr {intensity} = 
    let fBMR = toFloat bmr
    in case intensity of
              No               -> fBMR * 1.2
              Three            -> fBMR * 1.375
              Four             -> fBMR * 1.418
              Five             -> fBMR * 1.462
              Six              -> fBMR * 1.5
              FiveIntense      -> fBMR * 1.55
              EveryDay         -> fBMR * 1.637
              EveryDayIntense  -> fBMR * 1.725
              TwiceDaily       -> fBMR * 1.725
              DailyPhysicalJob -> fBMR * 1.9


-- BMR Equations
calcBMR : Info -> Int 
calcBMR info =
    case info.bmrEquation of
      MiffinStJeor -> calcMSJ info
      KatchMcArdle -> calcKMA info

-- Calculate basal metabolic rate using Mifflin-St Jeor
calcMSJ : Info -> Int
calcMSJ { gender, age, height, weight } =
    let baseVal = (10 * weight) + (6.25 * height) - (5 * (toFloat age))
    in
      case gender of
        Male   -> round (baseVal + 5)
        Female -> round (baseVal - 161)

-- Calculate basal metabolic rate using Katch-McArdle
calcKMA : Info -> Int
calcKMA { age, height, weight, fatPerc } =
    round (370.0 + (21.6 * ((1-(fatPerc/100)) * weight)))

---- VIEW ----
view : Model -> Html
view model =
    div
      [ class "container-fluid" ]
      [ div
        [ class "row" ]
        [ div
          [ class "col-md-8 col-md-offset-2" ]
          [ h1 [ class "text-center" ]  [ text "Simple IIFYM calculator" ]
          , br [] []
          , section
            [ id "infoInputs" ]
            [ infoInputs model.info ]
          , hr [ ] [ ]
          , section
            [ id "macroValues" ]
            [ macroValues model.macros ]
          ] 
        ]
      ]

infoInputs : Info -> Html
infoInputs info =
    Html.form
      [ class "form" ]
      [
       div
       [ class "row" ] 
       [
        div
        [ class "row" ]
        [ ageInput info
        , heightInput info
        , genderInput info
        ] 
       , div
        [ class "row" ]
        [ weightInput info
        , fatPercInput info
        , bmrInput info
        ] 
       ] 
      , div
       [ class "row" ]
       [ macroInput info ]
      , div
       [ class "row" ]
       [ intensityInput info ] 
      , div
       [ class "row" ]
       [ goalInput info ]
      ]
      

ageInput : Info -> Html
ageInput info =
    div [ class "form-group col-md-4" ]
            [ label
              [ for "age-input" ]
              [ text "Age: " ]
            , inputInt'
               { class = "form-control"
               , name = "age-input" 
               , placeholder = Nothing
               , value = info.age
               , min = Just 0
               , max = Nothing
               , step = Nothing
               , update = fieldUpdateContinuous
                          { onInput val = Signal.send updates (UpdateAge val) }
               }
            ] 

heightInput : Info -> Html
heightInput info = 
    div
      [ class "form-group col-md-4" ]
      [ label [ for "height-input" ] [ text "Height (cm): " ]
      , inputFloat'
        { class = "form-control"
        , name = "height-input"
        , placeholder = Nothing
        , value = info.height
        , min = Just 0.0
        , max = Nothing
        , step = Nothing
        , update = fieldUpdateContinuous
                   { onInput val = Signal.send updates (UpdateHeight val)}
        } 
      ] 

weightInput : Info -> Html
weightInput info =
    div
      [ class "form-group col-md-4" ]
      [ label [ for "weight-input" ] [ text "Weight (kg): " ]
      , inputFloat'
        { class = "form-control"
        , name = "height-input"
        , placeholder = Nothing
        , value = info.weight
        , min = Just 0.0
        , max = Nothing
        , step = Nothing
        , update = fieldUpdateContinuous
                   { onInput val = Signal.send updates (UpdateWeight val)}
        } 
      ] 

fatPercInput : Info -> Html
fatPercInput info =
    div
      [ class "form-group col-md-4" ]
      [ label [ for "fat-perc-input" ] [ text "Body fat (%): " ]
      , inputFloat'
        { class = "form-control"
        , name = "fat-perc-input"
        , placeholder = Nothing
        , value = info.fatPerc
        , min = Just 0.0
        , max = Nothing
        , step = Nothing
        , update = fieldUpdateContinuous
                   { onInput val = Signal.send updates (UpdateFatPerc val)}
        } 
      ] 

bmrInput : Info -> Html
bmrInput info =
    div
      [ class "form-group col-md-4" ]
      [ label [ for "bmr-equation-input" ] [ text "Which equation should be used?" ]
      , mkSelect info.bmrEquation 
                     [ ( "Miffin St Jeor (better for those with low body fat)"
                       , (UpdateBMREquation MiffinStJeor, MiffinStJeor))
                     , ("Katch McArdle (better for those who know their body fat percentage)", (UpdateBMREquation KatchMcArdle, KatchMcArdle))
                     ]
      ] 

genderInput : Info -> Html
genderInput info =
    div
      [ class "form-group col-md-4" ]
      [ label [ for "gender-input" ] [ text "What is your gender?" ]
      , mkSelect info.gender 
                     [ ("Male", (UpdateGender Male, Male))
                     , ("Female", (UpdateGender Female, Female))
                     ]
      ] 

intensityInput : Info -> Html
intensityInput info =
    div
      [ class "form-group" ]
      [ label [ for "intensity-input" ] [ text "How often do you exercise?" ]
      , mkSelect info.intensity 
                          [ ("Never", (UpdateIntensity No, No))
                          , ("Three times a week", (UpdateIntensity Three, Three))
                          , ("Four times a week", (UpdateIntensity Four, Four))
                          , ("Five times a week", (UpdateIntensity Five, Five))
                          , ("Six times a week", (UpdateIntensity Six, Six))
                          , ("Five times a week (intense)", (UpdateIntensity FiveIntense, FiveIntense))
                          , ("Every day", (UpdateIntensity EveryDay, EveryDay))
                          , ("Twice daily", (UpdateIntensity TwiceDaily, TwiceDaily))
                          , ("Every day, and I work a physical job", (UpdateIntensity DailyPhysicalJob, DailyPhysicalJob))
                          ] 
      ] 

macroInput : Info -> Html
macroInput info = 
    div
      [ class "form-group" ] 
      [ label
        [ for "macro-formula-input" ]
        [ text "Which formula should be used to calculate macros?" ]
      , mkSelect info.macroFormula 
                     [ ( "Textbook (P:25%, F:25%, C:50%)"
                       , (UpdateMacroFormula TextBook, TextBook))
                     , ( "Zone Diet (P:30%, F:30%, C:40%)"
                       , (UpdateMacroFormula ZoneDiet, ZoneDiet))
                     , ( "Low Fat (P:25%, F:15%, C:60%"
                       , (UpdateMacroFormula LowFat, LowFat))
                     ] 
      ]

goalInput : Info -> Html
goalInput info =
    div
      [ class "form-group" ] 
      [ label [ for "goal-input" ] [ text "What is your goal?" ]
      , mkSelect info.goal 
                          [ ("Cut Hard (-20% calories)", (UpdateGoal CutHard, CutHard))
                          , ("Cut (-15% calories)", (UpdateGoal Cut, Cut))
                          , ("Maintain", (UpdateGoal Maintain, Maintain))
                          , ("Bulk (+5% calories)", (UpdateGoal Bulk, Bulk))
                          , ("Bulk Hard (+10% calories)"
                            , (UpdateGoal BulkHard, BulkHard))
                          ] 
      ] 

mkSelect : a -> List (String, (Action, a)) -> Html
mkSelect infoVal fieldLst =
    let lookup = Dict.fromList fieldLst
        action a = case Dict.get a lookup of
                     Just x -> fst x
                     Nothing -> NoOp
    in 
      div [ class "form-group" ]
    [ 
      select [ class "form-control"
             , on "change" targetValue (Signal.send updates << (\val -> action val))
             ]
             (List.map (mkSelectField infoVal) fieldLst)
    ] 

mkSelectField : a -> (String, (Action, a)) -> Html
mkSelectField infoVal (labelText, (action, val)) =
    option [ on "click" targetValue (Signal.send updates << (\val -> Debug.log "onClick" action))
           , id labelText
           , selected (infoVal == val)
           ] [ text labelText ]
      
macroValues : Macros -> Html
macroValues {protein, fat, carbs}  = 
    div
      [ class "row" ]
      [ h2 [ class "text-center" ] [ text "Macro-nutrients (per-day)"]
      , br [] [] 
      , div
        [ class "col-md-4" ]
        [ p
          [ id "protein" ]
          [ b [ ] [ text "Protein: " ]
          , text ( toString (round protein) ++ " cals " ++ "(" ++ toString (round (protein / 4)) ++ " g)") ]
        ]
      , div
        [ class "col-md-4 coll-md-offset-4" ]
        [ p
          [ id "fat" ]
          [ b [ ] [ text "Fat: " ]
          , text ( toString (round fat) ++ " cals " ++ "(" ++ toString (round (carbs / 9)) ++ " g)") ]
        ]
      , div
        [ class "col-md-4 coll-md-offset-8" ]
        [ p
          [ id "carbs" ]
          [ b [ ] [ text "Carbohydrate: " ]
          , text ( toString (round carbs) ++ " cals " ++ "(" ++ toString (round (carbs / 4)) ++ " g)") ]
        ]
      ] 
    
--- SIGNALS ----
main : Signal Html
main = Signal.map view model

model : Signal Model
model = Signal.foldp update initialModel (Signal.subscribe updates)

initialModel : Model
initialModel =
    let info = { gender=Male
               , age=20
               , height=175.0
               , weight=75.0
               , fatPerc=20.0
               , intensity=Three
               , goal=Bulk
               , bmrEquation=MiffinStJeor
               , macroFormula=TextBook
               , units = Metric
               } 
    in { info=info, macros=calcMacros info } 
                 
updates : Signal.Channel Action
updates = Signal.channel NoOp
             
-- deal with localStorage
-- port getStorage : Maybe Mode

-- port setStorage : Signal Model
-- port setStorage = model
