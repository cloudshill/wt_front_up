module Views.Toolbars.TaskShow exposing (render)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Material
import Material.List as Lists
import Material.Menu as Menu
import Material.Options as Options exposing (cs, css, styled)
import Material.Toolbar as Toolbar
import Material.Typography as Typography


render :
    (Material.Msg m -> m)
    -> List Int
    -> Material.Mdoel m
    -> { pageX : Float, pageY : Float }
    -> m
    -> String
    -> Html m
render lift idx model scroll linkToHome title =
    Toolbar.view lift
        idx
        model
        [ Toolbar.flexible scroll.pageY
        , css "background" "#448aff"
        ]
        [ Toolbar.row [ css "height" "70px" ]
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ Toolbar.icon
                    [ Options.attribute (onClick linkToHome)
                    ]
                    "clear"
                ]
            , Toolbar.section
                [ Toolbar.alignEnd
                ]
                [ Toolbar.icon
                    [ Options.attribute (href "#")
                    , Menu.attach lift [ 0 ]
                    ]
                    "more_vert"
                , Menu.view lift
                    [ 0 ]
                    model
                    [ Menu.openFromTopRight
                    ]
                    (Menu.ul Lists.ul
                        []
                        [ Menu.li Lists.li
                            []
                            [ text "Delete"
                            ]
                        ]
                    )
                ]
            ]
        , Toolbar.row [ css "height" "25" ]
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ styled div
                    [ Typography.headline, cs "show-description" ]
                    [ text title
                    ]
                ]
            ]
        ]
