module Pages.VirtualTagTaskForm exposing
    ( config
    , initialModel
    )

import Data.Project as Project
import Data.Tag as Tag exposing (defaultTag)
import Data.Task as Task
    exposing
        ( DateField(..)
        , Period(..)
        , Priority(..)
        )
import Date exposing (Date)
import Main.Route as Route
import Pages.TaskForm exposing (initEdit)
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
    in
    { newConfig
        | tag = Just tag
        , tasks = Just model.requestedTasks
        , task = model.task
        , mobileConfig = ThreeColMobileConfig True True False
        , taskRoute = Route.TagTask model.slugOrQuery
        , topbarTitle = model.slugOrQuery
        , parentRoute = Route.Tags
    }


initialModel :
    String
    -> List Project.Project
    -> List Tag.Tag
    -> List Task.Task
    -> List Task.Task
    -> Date
    -> Task.Task
    -> ViewModel.Model m
initialModel slug projects tags tasks requestedTasks now task =
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
        , requestedTasks = requestedTasks
    }
