module Views.TaskForm.Model exposing (Error, Field(..), Model)

import Data.Task
    exposing
        ( DateField(..)
        , Period(..)
        , Priority(..)
        , Task
        )
import Date exposing (Date)
import DatePicker
import Material
import Views.TaskForm.ProjectPicker as ProjectPicker
import Views.TaskForm.TagsPicker as TagsPicker


type Field
    = Form
    | Description
    | DueField


type alias Error =
    ( Field, String )


type alias Model m =
    { mdc : Material.Model m
    , errors : List Error
    , isEditing : Bool
    , task : Task
    , uuid : String
    , description : String
    , project : Maybe String
    , tags : List String
    , due : Maybe Date
    , start : Maybe Date
    , end : Maybe Date
    , scheduled : Maybe Date
    , until : Maybe Date
    , wait : Maybe Date
    , datePicker : DatePicker.Model
    , showMoreDates : Bool
    , showProjectPicker : Bool
    , showTagsPicker : Bool
    , priority : Maybe Priority
    , radioPriority : Maybe Priority
    , recur : Maybe Period
    , radioRecur : Maybe Period
    , projectPicker : ProjectPicker.Model m
    , tagsPicker : TagsPicker.Model m
    , showPriorityDialog : Bool
    , showDueDialog : Bool
    , showStartDialog : Bool
    , showEndDialog : Bool
    , showScheduledDialog : Bool
    , showUntilDialog : Bool
    , showWaitDialog : Bool
    , showRecurDialog : Bool
    , showDirtyDialog : Bool
    , showValidationDialog : Bool
    , currentDate : Date
    , originalModel :
        { description : String
        , project : Maybe String
        , tags : List String
        , due : Maybe Date
        , start : Maybe Date
        , end : Maybe Date
        , scheduled : Maybe Date
        , until : Maybe Date
        , wait : Maybe Date
        , recur : Maybe Period
        , priority : Maybe Priority
        }
    }
