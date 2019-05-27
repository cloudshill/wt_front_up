module Pages.Tag exposing
    ( config
    , initialModel
    )

import Data.Project as Project
import Data.Tag exposing (Tag, defaultTag)
import Data.Task as WTask
import Main.Route as Route
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
        , mobileConfig = ThreeColMobileConfig True False True
        , fabRoute = Route.TagNewTask model.slugOrQuery
        , taskRoute = Route.TagTask model.slugOrQuery
        , topbarTitle = model.slugOrQuery
        , parentRoute = Route.Tags
    }


initialModel :
    String
    -> List Project.Project
    -> List Tag
    -> List WTask.Task
    -> Model m
initialModel slug projects tags tasks =
    let
        model =
            ViewModel.defaultModel
    in
    { model
        | slugOrQuery = slug
        , projects = projects
        , tags = tags
        , tasks = tasks
    }
