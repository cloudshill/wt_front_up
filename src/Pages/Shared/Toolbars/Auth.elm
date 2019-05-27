module Pages.Shared.Toolbars.Auth exposing (render)

import Html exposing (Html, text)
import Material
import Material.Toolbar as Toolbar
import Views.LoadingBar.LoadingBar as LoadingBar


render :
    (Material.Msg m -> m)
    -> Material.Index
    -> Material.Model m
    -> String
    -> Bool
    -> Html m
render lift idx model pageName isLoading =
    Toolbar.view lift
        idx
        model
        [ Toolbar.fixed
        ]
        [ Toolbar.row []
            [ Toolbar.section [ Toolbar.alignStart ]
                [ Toolbar.title [] [ text pageName ]
                ]
            , Toolbar.section [] []
            ]
        , LoadingBar.render isLoading "au-LoadingBarContainer"
        ]
