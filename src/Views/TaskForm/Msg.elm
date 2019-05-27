module Views.TaskForm.Msg exposing (ExternalMsg(..), Msg(..))

import Data.Task
    exposing
        ( DateField(..)
        , Period(..)
        , Priority(..)
        , Task
        )
import Data.TaskTagsProjects exposing (TaskTagsProjects)
import DatePicker
import Dom
import Http
import Main.Route as Route
import Material
import Views.TaskForm.ProjectPicker as ProjectPicker
import Views.TaskForm.TagsPicker as TagsPicker


type Msg m
    = Mdl (Material.Msg m)
    | ShowMoreDates
    | ShowProjectPicker
    | HideProjectPicker
    | ShowTagsPicker
    | HideTagsPicker
    | Reset DateField
    | DateSelected DateField
    | DatePickerMsg DatePicker.Msg
    | ProjectPickerMsg (ProjectPicker.Msg m)
    | TagsPickerMsg (TagsPicker.Msg m)
    | InitDatePicker DateField
    | SetPriority
    | SetDescription String
    | SetRadioPriority (Maybe Priority)
    | SetProject String
    | SetRadioRecur (Maybe Period)
    | CancelRecurDialog
    | CloseDialog
    | ShowPriorityDialog
    | ShowDueDialog
    | ShowStartDialog
    | ShowEndDialog
    | ShowScheduledDialog
    | ShowUntilDialog
    | ShowWaitDialog
    | ShowRecurDialog
    | CancelPriorityDialog
    | AddTag String
    | RemoveTag String
    | SetRecur
    | ResetRecur
    | ResetPriority
    | ResetProject
    | ResetTags
    | FocusResult (Result Dom.Error ())
    | GoTo Route.Route
    | GoBack
    | CreateCompleted (Result Http.Error TaskTagsProjects)
    | UpdateCompleted (Result Http.Error TaskTagsProjects)
    | Save
    | CancelForm


type ExternalMsg
    = NoOp
    | AppendTask Task
    | UpdateTask Task
    | UpdateTaskAfterResponse TaskTagsProjects
