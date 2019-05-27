module Pages.ReportTaskForm exposing
    ( config
    , initialModel
    )

-- import Date.Extra.Config.Config_en_us exposing (config)

import Data.Project as Project
import Data.Report as Report exposing (defaultReport)
import Data.Tag as Tag
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

        report =
            model.reports
                |> List.filter (\r -> r.slug == model.slugOrQuery)
                |> List.head
                |> Maybe.withDefault defaultReport
    in
    { newConfig
        | report = Just report
        , tasks = Just model.requestedTasks
        , reports = model.reports
        , task = model.task
        , mobileConfig = ThreeColMobileConfig True True False
        , taskRoute = Route.ReportTask model.slugOrQuery
        , topbarTitle = model.slugOrQuery
        , parentRoute = Route.Reports
    }


initialModel :
    String
    -> List Project.Project
    -> List Tag.Tag
    -> List Report.Report
    -> List Task.Task
    -> List Task.Task
    -> Date
    -> Task.Task
    -> ViewModel.Model m
initialModel slug projects tags reports tasks requestedTasks now task =
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
        , reports = reports
        , tasks = tasks
        , task = Just task
        , editTask = True
        , taskForm = Just taskFormModel
        , requestedTasks = requestedTasks
    }
