module Pages.Project exposing
    ( config
    , initialModel
    )

import Data.Project as Project exposing (defaultProject)
import Data.Tag as Tag
import Data.Task as Task
import Main.Route as Route exposing (Slug)
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

        project =
            model.projects
                |> List.filter (\p -> p.slug == model.slugOrQuery)
                |> List.head
                |> Maybe.withDefault defaultProject

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
        | project = Just project
        , tasks = Just projectTasks
        , mobileConfig = ThreeColMobileConfig True False True
        , fabRoute = Route.ProjectNewTask model.slugOrQuery
        , taskRoute = Route.ProjectTask model.slugOrQuery
        , topbarTitle = projectName
        , parentRoute = Route.Projects
    }


initialModel :
    Slug
    -> List Project.Project
    -> List Tag.Tag
    -> List Task.Task
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
