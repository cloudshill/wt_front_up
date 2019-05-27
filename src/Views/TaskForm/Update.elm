module Views.TaskForm.Update exposing (update)

import Data.Task as Task
    exposing
        ( DateField(..)
        , Period(..)
        , Priority(..)
        , taskToFields
        )
import Data.User exposing (User)
import Date exposing (Date)
import Date.Extra.Format as Format exposing (utcIsoString)
import DatePicker
import Dict
import Dom exposing (focus)
import Http
import Main.Route as Route
import Main.Util exposing ((=>), pair)
import Material
import Requests.Task
import Task
import Validate exposing (Validator, ifBlank, ifTrue, validate)
import Views.Helpers exposing (secondaryColor)
import Views.TaskForm.Model as Model exposing (Error, Model)
import Views.TaskForm.Msg exposing (ExternalMsg(..), Msg(..))
import Views.TaskForm.ProjectPicker as ProjectPicker
import Views.TaskForm.TagsPicker as TagsPicker


isRecurNotBlankAndDueIsBlank : Model m -> Bool
isRecurNotBlankAndDueIsBlank model =
    if model.recur == Nothing then
        False

    else
        model.due == Nothing


modelValidator : Validator Error (Model m)
modelValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .description (Model.Description => "Enter a description. A task must have a description.")
            , ifTrue (\model -> isRecurNotBlankAndDueIsBlank model) (Model.DueField => "Enter a due date. A recurring task must have a due date.")
            ]
        ]


update : User -> (Msg m -> m) -> Msg m -> Model m -> ( ( Model m, Cmd m ), ExternalMsg )
update user lift msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model => NoOp

        Save ->
            case validate modelValidator model of
                [] ->
                    let
                        defaultTask =
                            Task.defaultTask
                    in
                    if model.isEditing then
                        let
                            task =
                                model.task

                            updatedTaskWithoutFields =
                                { task
                                    | description = model.description
                                    , project = model.project
                                    , tags = model.tags
                                    , due = model.due
                                    , wait = model.wait
                                    , until = model.until
                                    , scheduled = model.scheduled
                                    , start = model.start
                                    , end = model.end
                                    , priority = model.priority
                                    , recurrence = model.recur
                                }

                            updatedTask =
                                { task | fields = taskToFields updatedTaskWithoutFields }

                            cmdRequest =
                                user.token
                                    |> Requests.Task.update model
                                    |> Http.send (lift << UpdateCompleted)
                        in
                        { model | errors = [] }
                            => cmdRequest
                            => UpdateTask updatedTask

                    else
                        let
                            taskWithoutFields =
                                { defaultTask
                                    | description = model.description
                                    , project = model.project
                                    , tags = model.tags
                                    , due = model.due
                                    , wait = model.wait
                                    , until = model.until
                                    , scheduled = model.scheduled
                                    , start = model.start
                                    , end = model.end
                                    , priority = model.priority
                                    , recurrence = model.recur
                                    , uuid = model.uuid
                                }

                            newTask =
                                { taskWithoutFields | fields = taskToFields taskWithoutFields }

                            cmdRequest =
                                user.token
                                    |> Requests.Task.create model
                                    |> Http.send (lift << CreateCompleted)
                        in
                        { model | errors = [] }
                            => Cmd.batch [ cmdRequest, Route.backOne ]
                            => AppendTask newTask

                errors ->
                    { model | errors = errors, showValidationDialog = True } => Cmd.none => NoOp

        CancelForm ->
            let
                currentModel =
                    { description = model.description
                    , project = model.project
                    , tags = model.tags
                    , due = model.due
                    , start = model.start
                    , end = model.end
                    , scheduled = model.scheduled
                    , until = model.until
                    , wait = model.wait
                    , recur = model.recur
                    , priority = model.priority
                    }
            in
            if currentModel == model.originalModel then
                model => Route.backOne => NoOp

            else
                { model | showDirtyDialog = True } => Cmd.none => NoOp

        CreateCompleted (Ok taskTagsProjects) ->
            model => Cmd.none => UpdateTaskAfterResponse taskTagsProjects

        CreateCompleted (Err _) ->
            { model
                | errors =
                    model.errors
                        ++ [ Model.Form
                                => "Server error while creating task"
                           ]
            }
                => Cmd.none
                => NoOp

        UpdateCompleted (Ok taskTagsProjects) ->
            model => Cmd.none => UpdateTaskAfterResponse taskTagsProjects

        UpdateCompleted (Err _) ->
            { model
                | errors =
                    model.errors
                        ++ [ Model.Form
                                => "Server error while updating task"
                           ]
            }
                => Cmd.none
                => NoOp

        GoTo route ->
            model => Route.modifyUrl route => NoOp

        GoBack ->
            ( model, Route.backOne ) => NoOp

        ResetRecur ->
            { model | recur = Nothing, radioRecur = Nothing } => Cmd.none => NoOp

        ResetPriority ->
            { model | priority = Nothing, radioPriority = Nothing }
                => Cmd.none
                => NoOp

        ResetProject ->
            { model | project = Nothing } => Cmd.none => NoOp

        ResetTags ->
            { model | tags = [] } => Cmd.none => NoOp

        SetRecur ->
            { model | recur = model.radioRecur, showRecurDialog = False } => Cmd.none => NoOp

        SetDescription description ->
            { model | description = description } => Cmd.none => NoOp

        SetRadioRecur period ->
            { model | radioRecur = period } => Cmd.none => NoOp

        CancelRecurDialog ->
            { model | radioRecur = model.recur, showRecurDialog = False } => Cmd.none => NoOp

        CancelPriorityDialog ->
            { model | radioPriority = model.priority, showPriorityDialog = False } => Cmd.none => NoOp

        SetProject project ->
            { model | project = Just project, showProjectPicker = False }
                => Cmd.none
                => NoOp

        AddTag tag ->
            let
                tagsPicker =
                    model.tagsPicker
            in
            { model
                | tags = tag :: model.tags
                , tagsPicker = { tagsPicker | needle = "" }
            }
                => Cmd.none
                => NoOp

        RemoveTag tag ->
            { model
                | tags = List.filter (\e -> e /= tag) model.tags
            }
                => Cmd.none
                => NoOp

        SetRadioPriority priority ->
            { model | radioPriority = priority }
                => Cmd.none
                => NoOp

        SetPriority ->
            { model | priority = model.radioPriority, showPriorityDialog = False } => Cmd.none => NoOp

        ShowMoreDates ->
            { model | showMoreDates = True } => Cmd.none => NoOp

        ShowProjectPicker ->
            { model | showProjectPicker = True }
                => Task.attempt (lift << FocusResult) (focus "tf-FilterProjectInput")
                => NoOp

        HideProjectPicker ->
            { model | showProjectPicker = False } => Cmd.none => NoOp

        FocusResult result ->
            case result of
                Err (Dom.NotFound _) ->
                    { model
                        | errors =
                            model.errors
                                ++ [ Model.Form
                                        => "Dom item search input not found"
                                   ]
                    }
                        => Cmd.none
                        => NoOp

                Ok () ->
                    model => Cmd.none => NoOp

        ShowTagsPicker ->
            { model | showTagsPicker = True }
                => Task.attempt (lift << FocusResult) (focus "tf-FilterProjectInput")
                => NoOp

        CloseDialog ->
            { model
                | showPriorityDialog = False
                , showDueDialog = False
                , showStartDialog = False
                , showEndDialog = False
                , showWaitDialog = False
                , showUntilDialog = False
                , showScheduledDialog = False
                , showRecurDialog = False
                , showDirtyDialog = False
                , showValidationDialog = False
            }
                => Cmd.none
                => NoOp

        ShowPriorityDialog ->
            { model | showPriorityDialog = True } => Cmd.none => NoOp

        ShowDueDialog ->
            let
                date =
                    model.due
            in
            case date of
                Nothing ->
                    { model
                        | datePicker =
                            DatePicker.init model.currentDate secondaryColor
                        , showDueDialog = True
                    }
                        => Cmd.none
                        => NoOp

                Just date ->
                    { model
                        | datePicker =
                            DatePicker.init date secondaryColor
                        , showDueDialog = True
                    }
                        => Cmd.none
                        => NoOp

        ShowStartDialog ->
            let
                date =
                    model.start
            in
            case date of
                Nothing ->
                    { model
                        | datePicker =
                            DatePicker.init model.currentDate secondaryColor
                        , showStartDialog = True
                    }
                        => Cmd.none
                        => NoOp

                Just date ->
                    { model
                        | datePicker =
                            DatePicker.init date secondaryColor
                        , showStartDialog = True
                    }
                        => Cmd.none
                        => NoOp

        ShowEndDialog ->
            let
                date =
                    model.end
            in
            case date of
                Nothing ->
                    { model
                        | datePicker =
                            DatePicker.init model.currentDate secondaryColor
                        , showEndDialog = True
                    }
                        => Cmd.none
                        => NoOp

                Just date ->
                    { model
                        | datePicker =
                            DatePicker.init date secondaryColor
                        , showEndDialog = True
                    }
                        => Cmd.none
                        => NoOp

        ShowWaitDialog ->
            let
                date =
                    model.wait
            in
            case date of
                Nothing ->
                    { model
                        | datePicker =
                            DatePicker.init model.currentDate secondaryColor
                        , showWaitDialog = True
                    }
                        => Cmd.none
                        => NoOp

                Just date ->
                    { model
                        | datePicker =
                            DatePicker.init date secondaryColor
                        , showWaitDialog = True
                    }
                        => Cmd.none
                        => NoOp

        ShowUntilDialog ->
            let
                date =
                    model.until
            in
            case date of
                Nothing ->
                    { model
                        | datePicker =
                            DatePicker.init model.currentDate secondaryColor
                        , showUntilDialog = True
                    }
                        => Cmd.none
                        => NoOp

                Just date ->
                    { model
                        | datePicker =
                            DatePicker.init date secondaryColor
                        , showUntilDialog = True
                    }
                        => Cmd.none
                        => NoOp

        ShowScheduledDialog ->
            let
                date =
                    model.scheduled
            in
            case date of
                Nothing ->
                    { model
                        | datePicker =
                            DatePicker.init model.currentDate secondaryColor
                        , showScheduledDialog = True
                    }
                        => Cmd.none
                        => NoOp

                Just date ->
                    { model
                        | datePicker =
                            DatePicker.init date secondaryColor
                        , showScheduledDialog = True
                    }
                        => Cmd.none
                        => NoOp

        ShowRecurDialog ->
            { model | showRecurDialog = True } => Cmd.none => NoOp

        HideTagsPicker ->
            { model | showTagsPicker = False } => Cmd.none => NoOp

        DatePickerMsg message ->
            { model
                | datePicker = DatePicker.update message model.datePicker
            }
                => Cmd.none
                => NoOp

        ProjectPickerMsg msg_ ->
            let
                ( newProjectPicker, effects ) =
                    ProjectPicker.update
                        (lift << ProjectPickerMsg)
                        msg_
                        model.projectPicker
            in
            { model | projectPicker = newProjectPicker } => effects => NoOp

        TagsPickerMsg msg_ ->
            let
                ( newTagsPicker, effects ) =
                    TagsPicker.update
                        (lift << TagsPickerMsg)
                        msg_
                        model.tagsPicker
            in
            { model | tagsPicker = newTagsPicker } => effects => NoOp

        Reset dateField ->
            case dateField of
                Due ->
                    { model | due = Nothing } => Cmd.none => NoOp

                Start ->
                    { model | start = Nothing } => Cmd.none => NoOp

                End ->
                    { model | end = Nothing } => Cmd.none => NoOp

                Scheduled ->
                    { model | scheduled = Nothing } => Cmd.none => NoOp

                Until ->
                    { model | until = Nothing } => Cmd.none => NoOp

                Wait ->
                    { model | wait = Nothing } => Cmd.none => NoOp

        DateSelected dateField ->
            case dateField of
                Due ->
                    { model
                        | due = Just (DatePicker.selectedDate model.datePicker)
                        , showDueDialog = False
                    }
                        => Cmd.none
                        => NoOp

                Start ->
                    { model
                        | start =
                            Just (DatePicker.selectedDate model.datePicker)
                        , showStartDialog = False
                    }
                        => Cmd.none
                        => NoOp

                End ->
                    { model
                        | end = Just (DatePicker.selectedDate model.datePicker)
                        , showEndDialog = False
                    }
                        => Cmd.none
                        => NoOp

                Scheduled ->
                    { model
                        | scheduled =
                            Just (DatePicker.selectedDate model.datePicker)
                        , showScheduledDialog = False
                    }
                        => Cmd.none
                        => NoOp

                Until ->
                    { model
                        | until =
                            Just (DatePicker.selectedDate model.datePicker)
                        , showUntilDialog = False
                    }
                        => Cmd.none
                        => NoOp

                Wait ->
                    { model
                        | wait = Just (DatePicker.selectedDate model.datePicker)
                        , showWaitDialog = False
                    }
                        => Cmd.none
                        => NoOp

        InitDatePicker dateField ->
            case dateField of
                Due ->
                    let
                        date =
                            model.due
                    in
                    case date of
                        Nothing ->
                            model => Cmd.none => NoOp

                        Just date ->
                            { model
                                | datePicker =
                                    DatePicker.init date secondaryColor
                            }
                                => Cmd.none
                                => NoOp

                Start ->
                    let
                        date =
                            model.start
                    in
                    case date of
                        Nothing ->
                            model => Cmd.none => NoOp

                        Just date ->
                            { model
                                | datePicker =
                                    DatePicker.init date secondaryColor
                            }
                                => Cmd.none
                                => NoOp

                End ->
                    let
                        date =
                            model.end
                    in
                    case date of
                        Nothing ->
                            model => Cmd.none => NoOp

                        Just date ->
                            { model
                                | datePicker =
                                    DatePicker.init date secondaryColor
                            }
                                => Cmd.none
                                => NoOp

                Scheduled ->
                    let
                        date =
                            model.scheduled
                    in
                    case date of
                        Nothing ->
                            model => Cmd.none => NoOp

                        Just date ->
                            { model
                                | datePicker =
                                    DatePicker.init date secondaryColor
                            }
                                => Cmd.none
                                => NoOp

                Until ->
                    let
                        date =
                            model.until
                    in
                    case date of
                        Nothing ->
                            model => Cmd.none => NoOp

                        Just date ->
                            { model
                                | datePicker =
                                    DatePicker.init date secondaryColor
                            }
                                => Cmd.none
                                => NoOp

                Wait ->
                    let
                        date =
                            model.until
                    in
                    case date of
                        Nothing ->
                            model => Cmd.none => NoOp

                        Just date ->
                            { model
                                | datePicker =
                                    DatePicker.init date secondaryColor
                            }
                                => Cmd.none
                                => NoOp
