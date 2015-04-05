module TripList where

import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy, lazy2)
import Json.Decode as Json
import List
import Maybe
import Signal
import String
import Window

---- MODEL ----
type alias Model =
    { tasks : List Task }

type alias Task =
    { description : String
    , category : String
    , uid : Int
    , active : Bool
    } 

type Action = NoOp

---- UPDATE ----
update : Action -> Model -> Model
update action model = model

---- VIEW ----
view : Model -> Html
view model =
    div [] [ section
          [ id "todoapp" ]
           [ lazy activeTasks model.tasks 
           , lazy inactiveTasks model.tasks ]
        ]

activeTasks : List Task -> Html
activeTasks tasks =
    div [ id "active" ]
            [ h1 []  [ text "Active Tasks" ]
            , ul []  (List.map itemHtml (List.filter (\{active} -> active==True) tasks))
            ] 

inactiveTasks : List Task -> Html
inactiveTasks tasks =
    div [ id "inactive" ]
            [ h1 []  [ text "Inactive Tasks" ]
            , ul []  (List.map itemHtml (List.filter (\{active} -> active==False) tasks))
            ] 

itemHtml : Task -> Html
itemHtml task = li [] [ text task.description, text task.category ]
--- SIGNALS ----

main : Signal Html
main = Signal.map view model

model : Signal Model
model = Signal.foldp update initialModel (Signal.subscribe updates)

initialModel : Model
initialModel = { tasks = initialTaskList }

initialTaskList : List Task
initialTaskList = [ { description = "ONE", category="test", uid = 1, active=False }
                  , { description = "TWO", category="test", uid = 2, active=True }] 

updates : Signal.Channel Action
updates = Signal.channel NoOp
        