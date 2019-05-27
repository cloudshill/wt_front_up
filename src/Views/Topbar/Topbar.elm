module Views.Topbar.Topbar exposing (render)

import Config exposing (config)
import Data.Context exposing (Context)
import Html exposing (Html, a, button, div, form, i, img, input, span, text)
import Html.Attributes
    exposing
        ( class
        , href
        , placeholder
        , src
        , target
        , type_
        , value
        )
import Html.Events as Events exposing (onClick, onInput, onSubmit)
import Main.Route as Route
import Material
import Material.Button as Button
import Material.Menu as Menu
import Material.Options as Options exposing (cs, styled)
import Material.Theme as Theme
import Material.TopAppBar as TopAppBar
import Material.Typography as Typography
import Views.LoadingBar.LoadingBar as LoadingBar


render :
    (Material.Msg m -> m)
    -> Material.Index
    -> Material.Model m
    -> String
    -> Bool
    -> Context
    -> Maybe (Html m)
    -> m
    -> (Route.Route -> m)
    -> m
    -> (String -> m)
    -> m
    -> String
    -> Html m
render lift idx model pageName isLoading activeContext backArrow toggleSearch goTo refreshData setSearchQuery submitSearch searchQuery =
    let
        backIcon =
            case backArrow of
                Nothing ->
                    span [] []

                Just html ->
                    html
    in
    div []
        [ TopAppBar.view lift
            idx
            model
            [ TopAppBar.fixed
            , cs "tb-TopAppBar"
            ]
            [ TopAppBar.section [ cs "tb-LogoSection", TopAppBar.alignStart ]
                [ backIcon
                , TopAppBar.title [ cs "show-for-small-only" ] [ text pageName ]
                , TopAppBar.title [ Options.onClick (goTo Route.Home), cs "show-for-large g-Logo grid-x align-middle" ]
                    [ img [ class "g-Logo_Image", src "/images/noun_project/wing.svg" ]
                        []
                    , text
                        "WingTask"
                    ]
                , img
                    [ Events.onClick (goTo Route.Home)
                    , class "g-Logo_Image show-for-medium-only"
                    , src "/images/noun_project/wing.svg"
                    ]
                    []
                ]
            , TopAppBar.section [ cs "show-for-small-only" ]
                (contextIndicator activeContext)
            , TopAppBar.section [ cs "show-for-medium" ]
                [ searchbar setSearchQuery submitSearch searchQuery
                ]
            , TopAppBar.section
                [ TopAppBar.alignEnd
                ]
                [ contextBadge activeContext
                , TopAppBar.actionItem [ Options.onClick refreshData, cs "show-for-medium" ] "refresh"
                , TopAppBar.actionItem [ Options.onClick (goTo Route.Settings), cs "show-for-medium" ] "settings"
                , Button.view lift
                    "tb-LogoutButton"
                    model
                    [ Theme.textPrimaryOnDark
                    , cs "show-for-medium"
                    , Options.onClick (goTo Route.Logout)
                    ]
                    [ text "Logout" ]
                , TopAppBar.actionItem [ cs "show-for-small-only", Options.onClick toggleSearch ] "search"
                , TopAppBar.actionItem [ cs "show-for-small-only", Menu.attach lift "tb-Menu" ] "more_vert"
                , Menu.view lift
                    "tb-Menu"
                    model
                    []
                    (Menu.ul
                        []
                        [ Menu.li
                            []
                            [ text ("Version " ++ config.version)
                            ]
                        , Menu.divider [] []
                        , Menu.li
                            [ Options.onClick refreshData ]
                            [ text "Refresh Data"
                            ]
                        , Menu.li
                            [ Options.onClick (goTo Route.Settings) ]
                            [ text "Settings"
                            ]
                        , Menu.li
                            []
                            [ a
                                [ href "https://docs.wingtask.com"
                                , class "tb-MenuLink"
                                , target "_blank"
                                ]
                                [ text "Go To Docs" ]
                            ]
                        , Menu.li
                            []
                            [ a
                                [ href "https://forum.wingtask.com"
                                , class "tb-MenuLink"
                                , target "_blank"
                                ]
                                [ text "Report a Bug / Request a Feature" ]
                            ]
                        , Menu.li
                            [ Options.onClick (goTo Route.Logout) ]
                            [ text "Logout"
                            ]
                        ]
                    )
                ]
            ]
        , LoadingBar.render isLoading ""
        ]


contextBadge : Context -> Html m
contextBadge context =
    if context.name == "none" then
        span [] []

    else
        div [ class "tb-ContextBadge show-for-medium" ]
            [ i [ class "material-icons tb-ContextBadge__icon" ] [ text "location_on" ]
            , span [ class "tb-ContextBadge__text" ] [ text context.name ]
            ]


contextIndicator : Context -> List (Html m)
contextIndicator context =
    if context.name == "none" then
        [ span [] []
        ]

    else
        [ div [ class "tb-CenterSection" ]
            [ styled
                div
                [ Typography.caption, cs "tb-ContextIndicator" ]
                [ i [ class "material-icons" ] [ text "location_on" ]
                , text context.name
                ]
            ]
        ]


searchbar : (String -> m) -> m -> String -> Html m
searchbar setSearchQuery submitSearch searchQuery =
    div [ class "tb-CenterSection" ]
        [ form
            [ class "sb-Container", onSubmit submitSearch ]
            [ button [ Events.onClick submitSearch, class "sb-Button__Search mdc-icon-button material-icons icon" ]
                [ text "search" ]
            , input [ class "sb-Input", placeholder "Search", type_ "search", onInput setSearchQuery, value searchQuery ]
                []
            , a [ Events.onClick (setSearchQuery ""), class "sb-Button__Clear material-icons icon" ]
                [ text "clear" ]
            ]
        ]
