module Pages.ReportTask exposing
    ( config
    , initialModel
    )

import Data.Report as Report exposing (defaultReport)
import Data.Task as Task
import Main.Route as Route
import Pages.Shared.Page exposing (FixedAdjust(..))
import Views.Helpers
    exposing
        ( ThreeColConfig
        , ThreeColMobileConfig
        , defaultThreeColConfig
        )
import Views.Model as ViewModel exposing (Model)


config : Model m -> ThreeColConfig
config model =
    let
        c =
            defaultThreeColConfig model

        uuid =
            case model.task of
                Nothing ->
                    ""

                Just task ->
                    task.uuid

        report =
            model.reports
                |> List.filter (\r -> r.slug == model.slugOrQuery)
                |> List.head
                |> Maybe.withDefault defaultReport
    in
    { c
        | report = Just report
        , tasks = Just model.requestedTasks
        , task = model.task
        , mobileConfig = ThreeColMobileConfig True True False
        , taskRoute = Route.ReportTask model.slugOrQuery
        , taskFormRoute = Route.ReportTaskEdit model.slugOrQuery uuid
        , topbarTitle = model.slugOrQuery
        , parentRoute = Route.Reports
    }


initialModel :
    String
    -> List Report.Report
    -> List Task.Task
    -> Task.Task
    -> Model m
initialModel slug reports requestedTasks task =
    let
        model =
            ViewModel.defaultModel
    in
    { model
        | slugOrQuery = slug
        , reports = reports
        , task = Just task
        , requestedTasks = requestedTasks
    }
