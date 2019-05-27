module Pages.TagTask exposing
    ( config
    , initialModel
    )

import Data.Project as Project
import Data.Tag as Tag exposing (defaultTag)
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
        newConfig =
            defaultThreeColConfig model

        uuid =
            case model.task of
                Nothing ->
                    ""

                Just task ->
                    task.uuid

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
        , taskFormRoute = Route.TagTaskEdit model.slugOrQuery uuid
        , topbarTitle = model.slugOrQuery
        , parentRoute = Route.Tags
    }


initialModel : String -> List Project.Project -> List Tag.Tag -> List Task.Task -> Task.Task -> Model m
initialModel slug projects tags tasks task =
    let
        model =
            ViewModel.defaultModel
    in
    { model
        | slugOrQuery = slug
        , projects = projects
        , tags = tags
        , tasks = tasks
        , task = Just task
    }
