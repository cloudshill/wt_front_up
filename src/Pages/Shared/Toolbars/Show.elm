module Pages.Shared.Toolbars.Show exposing (render)

import Html exposing (Html)
import Material
import Material.List as Lists
import Material.Menu as Menu
import Material.Msg
import Material.Options as Options
import Material.Toolbar as Toolbar


render :
    (Material.Msg.Msg m -> m) --lift
    -> Material.Msg.Index --idx
    -> Material.Model --model
    -> Html m --backArrow
    -> String --pageName
    -> m --toggleSearchBar
    -> (String -> m) --linkTo
    -> String --context
    -> m --showSnackBar
    -> m --toggleBlankState
    -> Html m
render lift idx model backArrow pageName toggleSearchBar linkTo context showSnackbar toggleBlankState =
    Toolbar.render lift
        idx
        model
        [ Toolbar.fixed
        ]
        [ Toolbar.row
            []
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ backArrow
                , Toolbar.title
                    []
                    [ text pageName
                    ]
                ]
            , Toolbar.section
                []
                (contextIndicator context)
            , Toolbar.section
                [ Toolbar.alignEnd
                ]
                [ Toolbar.icon
                    [ Options.onClick toggleSearchBar
                    ]
                    "search"
                , Toolbar.icon
                    [ Options.attribute (href "#")
                    , Menu.attach lift [ 0 ]
                    ]
                    "more_vert"
                , Menu.render lift
                    [ 0 ]
                    model
                    [ Menu.openFromTopRight
                    ]
                    (Menu.ul Lists.ul
                        []
                        [ Menu.li Lists.li
                            [ Options.onClick (linkTo "#/settings")
                            ]
                            [ text "Settings"
                            ]
                        , Menu.li Lists.li
                            [ Options.onClick (linkTo "#/login")
                            ]
                            [ text "Login"
                            ]
                        , Menu.li Lists.li
                            [ Options.onClick (linkTo "#/signup")
                            ]
                            [ text "Signup"
                            ]
                        , Menu.li Lists.li
                            [ Options.onClick
                                (linkTo "#/change_password/4343444444")
                            ]
                            [ text "Change Password"
                            ]
                        , Menu.li Lists.li
                            [ Options.onClick showSnackbar
                            ]
                            [ text "Show snackbar"
                            ]
                        , Menu.li Lists.li
                            [ Options.onClick toggleBlankState
                            ]
                            [ text "Toggle Blank State"
                            ]
                        ]
                    )
                ]
            ]
        ]


contextIndicator : String -> List (Html m)
contextIndicator context =
    if context == "" then
        [ span [] []
        ]

    else
        [ div [ class "context-indicator" ]
            [ i [ class "material-icons" ] [ text "location_on" ]
            , text context
            ]
        ]
