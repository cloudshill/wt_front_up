module Pages.Home exposing
    ( config
    , initialModel
    )

import Data.Project as Project
import Data.Tag as Tag
import Data.Task as Task
import Views.Helpers
    exposing
        ( ThreeColConfig
        , ThreeColMobileConfig
        , defaultThreeColConfig
        )
import Views.Model as ViewModel exposing (Model)


initialModel : List Project.Project -> List Tag.Tag -> List Task.Task -> Model m
initialModel projects tags tasks =
    let
        model =
            ViewModel.defaultModel
    in
    { model | projects = projects, tags = tags, tasks = tasks }


config : Model m -> ThreeColConfig
config model =
    let
        newConfig =
            defaultThreeColConfig model
    in
    { newConfig
        | tasks = Just model.tasks
        , mobileConfig = ThreeColMobileConfig True False True
        , topbarTitle = "Home"
    }
