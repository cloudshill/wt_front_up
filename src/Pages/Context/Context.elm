module Pages.Context.Context exposing
    ( config
    , initialModel
    )

import Data.Context as Context exposing (defaultContext)
import Main.Route as Route exposing (Slug)
import Pages.Shared.Page exposing (FixedAdjust(..))
import Views.Helpers exposing (ThreeColConfig, ThreeColMobileConfig, defaultThreeColConfig)
import Views.Model as ViewModel exposing (Model)


config : Model m -> ThreeColConfig
config model =
    let
        context =
            model.contexts
                |> List.filter (\c -> c.slug == model.slugOrQuery)
                |> List.head
                |> Maybe.withDefault defaultContext

        c =
            defaultThreeColConfig model
    in
    { c
        | mobileConfig = ThreeColMobileConfig True True False
        , topbarTitle = "Contexts"
        , fabRoute = Route.NewContext
        , parentRoute = Route.Contexts
        , contexts = model.contexts
        , context = Just context
    }


initialModel : Slug -> List Context.Context -> Model m
initialModel slug contexts =
    let
        model =
            ViewModel.defaultModel
    in
    { model
        | contexts = contexts
        , slugOrQuery = slug
    }
