module Pages.ProjectTask exposing
    ( config
    , initialModel
    )

import Data.Project as Project
import Data.Tag as Tag
import Data.Task as Task
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
        c =
            defaultThreeColConfig model

        uuid =
            case model.task of
                Nothing ->
                    ""

                Just task ->
                    task.uuid

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
        , taskFormRoute = Route.ProjectTaskEdit model.slugOrQuery uuid
        , topbarTitle = projectName
        , parentRoute = Route.Projects
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
