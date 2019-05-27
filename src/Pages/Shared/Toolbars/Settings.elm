module Views.Toolbars.Settings exposing (render)

import Html exposing (Html)
import Material
import Material.Msg
import Material.Options exposing (css)
import Material.Toolbar as Toolbar


render :
    (Material.Msg.Msg m -> m)
    -> Material.Msg.Index
    -> Material.Model
    -> Html m
    -> Html m
render lift idx model backArrow =
    Toolbar.render lift
        idx
        model
        [ Toolbar.fixed
        , css "background" "#448aff"
        ]
        [ Toolbar.row
            []
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ backArrow
                , Toolbar.title
                    []
                    [ text "Settings"
                    ]
                ]
            ]
        ]
