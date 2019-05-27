module Pages.Projects exposing (config, initialModel)

import Data.Project as Project
import Data.Tag as Tag
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
    in
    { c
        | parentRoute = Route.Projects
        , mobileConfig = ThreeColMobileConfig False True True
        , topbarTitle = "Projects"
    }


initialModel : List Project.Project -> List Tag.Tag -> List Task.Task -> Model m
initialModel projects tags tasks =
    let
        model =
            ViewModel.defaultModel
    in
    { model | projects = projects, tags = tags, tasks = tasks }
