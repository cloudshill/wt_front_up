module Pages.ProjectTaskForm exposing
    ( config
    , initialEditModel
    , initialNewModel
    )

import Data.Project as Project
import Data.Tag as Tag
import Data.Task as Task exposing (defaultTask)
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
        c =
            defaultThreeColConfig model

        maybeProject =
            model.projects
                |> List.filter (\p -> p.slug == model.slugOrQuery)
                |> List.head

        projectName =
            model.projects
                |> List.filter (\p -> p.slug == model.slugOrQuery)
                |> List.map (\p -> p.extendedName)
                |> List.head
                |> Maybe.withDefault ""

        projectTasks =
            model.tasks
                |> List.filter
                    (\t ->
                        String.startsWith
                            projectName
                            (t.project |> Maybe.withDefault "")
                    )
    in
    { c
        | project = maybeProject
        , tasks = Just projectTasks
        , task = model.task
        , mobileConfig = ThreeColMobileConfig True True False
        , fabRoute = Route.ProjectNewTask model.slugOrQuery
        , taskRoute = Route.ProjectTask model.slugOrQuery
        , topbarTitle = model.slugOrQuery
        , parentRoute = Route.Projects
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
                (Just slug)
                Nothing
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
