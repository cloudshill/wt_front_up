module Pages.Shared.Toolbars.BottomNav.BottomNav exposing (render)

import Html exposing (Html, a, div, span, text)
import Html.Attributes exposing (class, classList)
import Main.Route as Route exposing (Route)
import Material
import Material.Bottombar as Bottombar
import Material.Icon as Icon
import Material.Options as Options
import Pages.Shared.Page exposing (CurrentPage(..), isCurrentPage)


render :
    (Material.Msg m -> m)
    -> Material.Model m
    -> Route
    -> Html m
render lift model currentRoute =
    Bottombar.view lift
        "g-Bottombar"
        model
        [ Bottombar.fixed
        , Options.attribute (class "show-for-small-only")
        ]
        [ Bottombar.section
            []
            [ div
                [ classList
                    [ ( "icon-text", True )
                    , ( "active", isCurrentPage Home currentRoute )
                    ]
                ]
                [ a [ Route.href Route.Home ]
                    [ Icon.view [ Bottombar.icon ] "home"
                    , span [] [ text "home" ]
                    ]
                ]
            , div
                [ classList
                    [ ( "icon-text", True )
                    , ( "active", isCurrentPage Projects currentRoute )
                    ]
                ]
                [ a [ Route.href Route.Projects ]
                    [ Icon.view [ Bottombar.icon ] "work"
                    , span [] [ text "projects" ]
                    ]
                ]
            , div
                [ classList
                    [ ( "icon-text", True )
                    , ( "active", isCurrentPage Tags currentRoute )
                    ]
                ]
                [ a [ Route.href Route.Tags ]
                    [ Icon.view [ Bottombar.icon ] "label"
                    , span [] [ text "tags" ]
                    ]
                ]
            , div
                [ classList
                    [ ( "icon-text", True )
                    , ( "active", isCurrentPage Reports currentRoute )
                    ]
                ]
                [ a [ Route.href Route.Reports ]
                    [ Icon.view [ Bottombar.icon ] "view_list"
                    , span [] [ text "reports" ]
                    ]
                ]
            ]
        ]
