module Pages.HomeTask exposing
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


config : ViewModel.Model m -> ThreeColConfig
config model =
    let
        uuid =
            case model.task of
                Nothing ->
                    ""

                Just task ->
                    task.uuid

        newConfig =
            defaultThreeColConfig model
    in
    { newConfig
        | tasks = Just model.tasks
        , mobileConfig = ThreeColMobileConfig True True False
        , topbarTitle = "Home"
        , task = model.task
        , taskFormRoute = Route.HomeTaskEdit uuid
    }


initialModel :
    String
    -> List Project.Project
    -> List Tag.Tag
    -> List Task.Task
    -> Task.Task
    -> Model m
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
