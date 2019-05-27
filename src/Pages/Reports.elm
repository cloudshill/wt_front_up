module Pages.Reports exposing (config, initialModel)

import Data.Report as Report
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
    in
    { newConfig
        | parentRoute = Route.Reports
        , mobileConfig = ThreeColMobileConfig False True True
        , topbarTitle = "Reports"
    }


initialModel : List Report.Report -> Model m
initialModel reports =
    let
        model =
            ViewModel.defaultModel
    in
    { model | reports = reports }
