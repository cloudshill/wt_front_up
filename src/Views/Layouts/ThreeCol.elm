module Views.Layouts.ThreeCol exposing (view)

import Html exposing (Html, div, main_)
import Html.Attributes exposing (class)
import Main.Route exposing (Route)
import Pages.Shared.Page exposing (CurrentPage(..))
import Views.Helpers exposing (ThreeColConfig)
import Views.SidebarNav.SidebarNav exposing (sidebarNavCol)


view :
    ThreeColConfig
    -> Html m
    -> Html m
    -> Html m
    -> (Route -> m)
    -> Html m
view config firstCol secondCol thirdCol goTo =
    main_ [ class "main g-XlargeGrid-y grid-frame cell-block-container" ]
        [ div [ class "grid-x grid-margin-x cell main-content" ]
            [ sidebarNavCol goTo config
            , firstCol
            , secondCol
            , thirdCol
            ]
        ]
