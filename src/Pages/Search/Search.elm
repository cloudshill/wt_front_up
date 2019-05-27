module Pages.Search.Search exposing
    ( config
    , init
    )

import Data.Session exposing (Session)
import Http
import Material
import Pages.Shared.Page as Page
import Requests.Bucket.Task
import Task
import Views.Errored.Errored as Errored exposing (PageLoadError, pageLoadError)
import Views.Helpers
    exposing
        ( ThreeColConfig
        , ThreeColMobileConfig
        , defaultThreeColConfig
        )
import Views.Model exposing (Model, defaultActiveContext)


config : Model m -> ThreeColConfig
config model =
    let
        newConfig =
            defaultThreeColConfig model

        query =
            Just model.slugOrQuery

        queryText =
            case query of
                Nothing ->
                    "Home"

                Just query ->
                    "Search result for '" ++ query ++ "'"
    in
    { newConfig
        | tasks = Just model.requestedTasks
        , mobileConfig = ThreeColMobileConfig True False True
        , topbarTitle = queryText
        , query = query
    }


init : Session -> String -> Task.Task PageLoadError (Model m)
init session query =
    let
        maybeAuthToken =
            Maybe.map .token session.user

        loadTasks =
            Requests.Bucket.Task.search maybeAuthToken query
                |> Http.toTask

        handleLoadError _ =
            "There was a problem with loading bucket."
                |> pageLoadError Page.Home
    in
    Task.map
        (Model Material.defaultModel
            []
            query
            []
            []
            []
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
