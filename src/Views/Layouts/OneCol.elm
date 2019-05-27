module Views.Layouts.OneCol exposing (view)

import Html exposing (Html, div, main_)
import Html.Attributes exposing (class)
import Main.Route exposing (Route)
import Views.Helpers exposing (ThreeColConfig)
import Views.SidebarNav.SidebarNav exposing (sidebarNavCol)


view : Html m -> (Route -> m) -> ThreeColConfig -> Html m
view content goTo config =
    main_ [ class "main grid-y grid-frame cell-block-container" ]
        [ div [ class "grid-x grid-margin-x cell main-content" ]
            [ sidebarNavCol goTo config
            , content
            ]
        ]
