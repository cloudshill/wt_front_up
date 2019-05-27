module Pages.TaskForm exposing (initEdit, initNew)

import Data.Project as Project
import Data.Tag as Tag
import Data.Task as Task
    exposing
        ( DateField(..)
        , Period(..)
        , Priority(..)
        )
import Date exposing (Date)
import DatePicker
import Material
import Uuid exposing (Uuid)
import Views.Helpers exposing (secondaryColor)
import Views.TaskForm.Model as TaskFormModel
import Views.TaskForm.ProjectPicker as ProjectPicker
import Views.TaskForm.TagsPicker as TagsPicker


initNew :
    List Project.Project
    -> List Tag.Tag
    -> Date
    -> Maybe String
    -> Maybe String
    -> Uuid
    -> TaskFormModel.Model m
initNew projects tags now defaultProjectSlug defaultTagSlug uuid =
    let
        defaultProject =
            case defaultProjectSlug of
                Just slug ->
                    let
                        projectName =
                            projects
                                |> List.filter (\p -> p.slug == slug)
                                |> List.map (\p -> p.extendedName)
                                |> List.head
                                |> Maybe.withDefault ""
                    in
                    Just projectName

                Nothing ->
                    Nothing

        defaultTags =
            case defaultTagSlug of
                Just slug ->
                    let
                        tagName =
                            tags
                                |> List.filter (\t -> t.slug == slug)
                                |> List.map (\t -> t.name)
                                |> List.head
                    in
                    case tagName of
                        Just tagName ->
                            [ tagName ]

                        Nothing ->
                            []

                Nothing ->
                    []
    in
    { mdc = Material.defaultModel
    , errors = []
    , isEditing = False
    , task = Task.defaultTask
    , uuid = Uuid.toString uuid
    , description = ""
    , project = defaultProject
    , due = Nothing
    , tags = defaultTags
    , start = Nothing
    , end = Nothing
    , scheduled = Nothing
    , until = Nothing
    , wait = Nothing
    , datePicker =
        DatePicker.init
            now
            secondaryColor
    , showMoreDates = False
    , showProjectPicker = False
    , showTagsPicker = False
    , priority = Nothing
    , radioPriority = Nothing
    , recur = Nothing
    , radioRecur = Nothing
    , projectPicker = ProjectPicker.initialModel projects
    , tagsPicker = TagsPicker.initialModel tags
    , showPriorityDialog = False
    , showDueDialog = False
    , showStartDialog = False
    , showEndDialog = False
    , showScheduledDialog = False
    , showUntilDialog = False
    , showWaitDialog = False
    , showRecurDialog = False
    , showDirtyDialog = False
    , showValidationDialog = False
    , currentDate = now
    , originalModel =
        { description = ""
        , project = Nothing
        , tags = []
        , due = Nothing
        , start = Nothing
        , end = Nothing
        , scheduled = Nothing
        , until = Nothing
        , wait = Nothing
        , recur = Nothing
        , priority = Nothing
        }
    }


initEdit : Task.Task -> List Project.Project -> List Tag.Tag -> Date -> TaskFormModel.Model m
initEdit task projects tags now =
    { mdc = Material.defaultModel
    , errors = []
    , isEditing = True
    , task = task
    , uuid = task.uuid
    , description = task.description
    , project = task.project
    , due = task.due
    , tags = task.tags
    , start = task.start
    , end = task.end
    , scheduled = task.scheduled
    , until = task.until
    , wait = task.wait
    , datePicker =
        DatePicker.init
            now
            secondaryColor
    , showMoreDates = False
    , showProjectPicker = False
    , showTagsPicker = False
    , priority = task.priority
    , radioPriority = task.priority
    , recur = task.recurrence
    , radioRecur = task.recurrence
    , projectPicker = ProjectPicker.initialModel projects
    , tagsPicker = TagsPicker.initialModel tags
    , showPriorityDialog = False
    , showDueDialog = False
    , showStartDialog = False
    , showEndDialog = False
    , showScheduledDialog = False
    , showUntilDialog = False
    , showWaitDialog = False
    , showRecurDialog = False
    , showDirtyDialog = False
    , showValidationDialog = False
    , currentDate = now
    , originalModel =
        { description = task.description
        , project = task.project
        , tags = task.tags
        , due = task.due
        , start = task.start
        , end = task.end
        , scheduled = task.scheduled
        , until = task.until
        , wait = task.wait
        , recur = task.recurrence
        , priority = task.priority
        }
    }
