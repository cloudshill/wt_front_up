module Pages.VirtualTag exposing
    ( config
    , init
    )

import Data.Project as Project
import Data.Session exposing (Session)
import Data.Tag as Tag exposing (defaultTag)
import Data.Task exposing (Task)
import Http
import Main.Route as Route
import Material
import Pages.Shared.Page as Page
import Requests.Bucket.Task
import Task
import Views.Errored.Errored as Errored exposing (PageLoadError, pageLoadError)
import Views.Helpers
    exposing
        ( ThreeColConfig
        , ThreeColMobileConfig
        , buildUserTags
        , buildVirtualTags
        , defaultThreeColConfig
        )
import Views.Model exposing (Model, defaultActiveContext)


type alias ViewTag =
    { tag : Tag.Tag
    , taskCount : Int
    }


config : Model m -> List Tag.Tag -> List Task -> ThreeColConfig
config model tags tasks =
    let
        uuid =
            case model.task of
                Nothing ->
                    ""

                Just task ->
                    task.uuid

        newConfig =
            defaultThreeColConfig model

        tag =
            tags
                |> List.filter (\t -> t.slug == model.slugOrQuery)
                |> List.head
                |> Maybe.withDefault defaultTag
    in
    { newConfig
        | tag = Just tag
        , userTags = buildUserTags tags tasks
        , virtualTags = buildVirtualTags tags
        , tasks = Just model.requestedTasks
        , mobileConfig = ThreeColMobileConfig True False True
        , taskRoute = Route.VirtualTagTask model.slugOrQuery
        , taskFormRoute = Route.TagTaskEdit model.slugOrQuery uuid
        , topbarTitle = model.slugOrQuery
        , parentRoute = Route.Tags
    }


init :
    Session
    -> String
    -> List Project.Project
    -> List Tag.Tag
    -> Task.Task PageLoadError (Model m)
init session slug projects tags =
    let
        maybeAuthToken =
            Maybe.map .token session.user

        loadTasks =
            Requests.Bucket.Task.list maybeAuthToken
                "virtual_tags"
                slug
                |> Http.toTask

        handleLoadError _ =
            "There was a problem with loading bucket."
                |> pageLoadError Page.Home
    in
    Task.map
        (Model Material.defaultModel
            []
            slug
            projects
            tags
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
