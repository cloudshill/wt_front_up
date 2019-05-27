module Pages.Report exposing
    ( config
    , init
    )

import Data.Report exposing (Report, defaultReport)
import Data.Session exposing (Session)
import Http
import Main.Route as Route
import Material
import Pages.Shared.Page as Page
import Requests.Report.Task
import Task
import Views.Errored.Errored exposing (PageLoadError, pageLoadError)
import Views.Helpers
    exposing
        ( ThreeColConfig
        , ThreeColMobileConfig
        , defaultThreeColConfig
        )
import Views.Model exposing (Model, defaultActiveContext)


config : Model m -> List Report -> ThreeColConfig
config model reports =
    let
        c =
            defaultThreeColConfig model

        report =
            reports
                |> List.filter (\r -> r.slug == model.slugOrQuery)
                |> List.head
                |> Maybe.withDefault defaultReport
    in
    { c
        | report = Just report
        , tasks = Just model.requestedTasks
        , mobileConfig = ThreeColMobileConfig True False True
        , taskRoute = Route.ReportTask model.slugOrQuery
        , topbarTitle = model.slugOrQuery
        , reports = reports
        , parentRoute = Route.Reports
    }


init : Session -> String -> List Report -> Task.Task PageLoadError (Model m)
init session slug reports =
    let
        maybeAuthToken =
            Maybe.map .token session.user

        loadTasks =
            Requests.Report.Task.list maybeAuthToken slug
                |> Http.toTask

        handleLoadError _ =
            "There was a problem with loading bucket."
                |> pageLoadError Page.Home
    in
    Task.map
        (Model Material.defaultModel
            []
            slug
            []
            []
            reports
            []
            Nothing
            defaultActiveContext
            []
            []
            Nothing
            False
            Nothing
            ""
        )
        loadTasks
        |> Task.mapError handleLoadError
