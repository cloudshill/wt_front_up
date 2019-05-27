module Views.TaskDetail.Topbar exposing (render)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Material
import Material.Icon as Icon
import Material.Menu as Menu
import Material.Options as Options exposing (cs, styled)
import Material.Toolbar as Toolbar
import Material.Typography as Typography


render :
    (Material.Msg m -> m) --lift
    -> Material.Index --idx
    -> Material.Model m --model
    -> m --linkToHome
    -> String --title
    -> m --Mark Done
    -> m --Delete
    -> Html m
render lift idx model linkToHome title markDone delete =
    Toolbar.view lift
        idx
        model
        [ Toolbar.flexible
        , cs "td-SmallTopbar"
        ]
        [ Toolbar.row [ cs "td-SmallTopbar_Row" ]
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ Icon.view
                    [ Toolbar.icon
                    , Options.onClick linkToHome
                    ]
                    "clear"
                ]
            , Toolbar.section
                [ Toolbar.alignEnd
                ]
                [ Icon.view
                    [ Toolbar.icon
                    , Options.attribute (href "#")
                    , Menu.attach lift "tt-Menu"
                    ]
                    "more_vert"
                , Menu.view lift
                    "tt-Menu"
                    model
                    []
                    (Menu.ul
                        []
                        [ Menu.li
                            [ Options.onClick markDone ]
                            [ text "Mark Done"
                            ]
                        , Menu.li
                            [ Options.onClick delete ]
                            [ text "Delete"
                            ]
                        ]
                    )
                ]
            ]
        , Toolbar.row []
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ styled div
                    [ Typography.headline, cs "td-SmallTopbar_Description" ]
                    [ text title
                    ]
                ]
            ]
        ]
