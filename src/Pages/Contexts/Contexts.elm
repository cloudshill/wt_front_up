module Pages.Contexts.Contexts exposing
    ( config
    , initialModel
    )

import Data.Context as Context
import List.Extra
import Main.Route as Route
import Pages.Shared.Page exposing (FixedAdjust(..))
import SelectList exposing (SelectList)
import Views.Helpers exposing (ThreeColConfig, ThreeColMobileConfig, defaultThreeColConfig)
import Views.Model as ViewModel exposing (Model)


config : Model m -> ThreeColConfig
config model =
    let
        c =
            defaultThreeColConfig model
    in
    { c
        | mobileConfig = ThreeColMobileConfig True False True
        , topbarTitle = "Contexts"
        , fabRoute = Route.NewContext
        , parentRoute = Route.Contexts
    }


initialModel : List Context.Context -> Model m
initialModel contexts =
    let
        activeContext =
            mapContextsToSelectList contexts

        model =
            ViewModel.defaultModel
    in
    { model | contexts = contexts, activeContext = activeContext }


mapContextsToSelectList : List Context.Context -> SelectList Int
mapContextsToSelectList contexts =
    let
        l =
            SelectList.fromLists [] 0 (List.range 0 (List.length contexts - 1))

        activeContext =
            Context.findActiveContext contexts

        idx =
            List.Extra.findIndex (\c -> c == activeContext) contexts |> Maybe.withDefault 0
    in
    SelectList.select (\u -> u == idx) l
