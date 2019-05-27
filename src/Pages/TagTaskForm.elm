module Pages.TagTaskForm exposing
    ( config
    , initialEditModel
    , initialNewModel
    )

import Data.Project as Project
import Data.Tag as Tag exposing (defaultTag)
import Data.Task as Task
    exposing
        ( DateField(..)
        , Period(..)
        , Priority(..)
        , defaultTask
        )
import Date exposing (Date)
import Main.Route as Route
import Pages.TaskForm exposing (initEdit, initNew)
import Uuid exposing (Uuid)
import Views.Helpers
    exposing
        ( ThreeColConfig
        , ThreeColMobileConfig
        , defaultThreeColConfig
        )
import Views.Model as ViewModel


config : ViewModel.Model m -> ThreeColConfig
config model =
    let
        newConfig =
            defaultThreeColConfig model

        tag =
            model.tags
                |> List.filter (\t -> t.slug == model.slugOrQuery)
                |> List.head
                |> Maybe.withDefault defaultTag

        tagTasks =
            model.tasks
                |> List.filter
                    (\task ->
                        List.any (\tag -> tag == model.slugOrQuery) task.tags
                    )
    in
    { newConfig
        | tag = Just tag
        , tasks = Just tagTasks
        , task = model.task
        , mobileConfig = ThreeColMobileConfig True True False
        , fabRoute = Route.TagNewTask model.slugOrQuery
        , taskRoute = Route.TagTask model.slugOrQuery
        , topbarTitle = model.slugOrQuery
        , parentRoute = Route.Tags
    }


initialNewModel :
    String
    -> List Project.Project
    -> List Tag.Tag
    -> List Task.Task
    -> Date
    -> Uuid
    -> ViewModel.Model m
initialNewModel slug projects tags tasks now uuid =
    let
        model =
            ViewModel.defaultModel

        taskFormModel =
            initNew
                projects
                tags
                now
                Nothing
                (Just slug)
                uuid
    in
    { model
        | slugOrQuery = slug
        , projects = projects
        , tags = tags
        , tasks = tasks
        , task = Just defaultTask
        , editTask = True
        , taskForm = Just taskFormModel
    }


initialEditModel :
    String
    -> List Project.Project
    -> List Tag.Tag
    -> List Task.Task
    -> Date
    -> Task.Task
    -> ViewModel.Model m
initialEditModel slug projects tags tasks now task =
    let
        model =
            ViewModel.defaultModel

        taskFormModel =
            initEdit task projects tags now
    in
    { model
        | slugOrQuery = slug
        , projects = projects
        , tags = tags
        , tasks = tasks
        , task = Just task
        , editTask = True
        , taskForm = Just taskFormModel
    }
