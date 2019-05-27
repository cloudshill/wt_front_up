module Views.SidebarNav.SidebarNav exposing (sidebarNavCol)

import Html exposing (Html, a, button, div, i, span, text)
import Html.Attributes exposing (attribute, class, classList)
import Html.Events exposing (onClick)
import Main.Route as Route exposing (Route)
import Pages.Shared.Page exposing (CurrentPage(..), isCurrentPage)
import Views.Helpers exposing (ThreeColConfig)


sidebarNavCol : (Route -> m) -> ThreeColConfig -> Html m
sidebarNavCol goTo config =
    let
        fabText =
            if config.parentRoute == Route.Contexts then
                "Context"

            else
                "Task"
    in
    div [ class "g-SidebarNav cell medium-2 cell-block-y show-for-medium" ]
        [ div [ class "si-FabContainer" ]
            [ button [ onClick (goTo config.fabRoute), attribute "aria-label" "Create", class "si-Fab" ]
                [ span [ class "material-icons mdc-fab__icon" ]
                    [ text "add" ]
                , span [ class "mdc-fab__label show-for-large" ]
                    [ text fabText ]
                ]
            ]
        , view goTo config.parentRoute
        ]


view : (Route -> m) -> Route.Route -> Html m
view goTo currentRoute =
    div [ class "si-NavList mdc-list" ]
        [ a
            [ classList
                [ ( "mdc-list-item si-NavItem", True )
                , ( "mdc-list-item--selected si-NavItem--selected", isCurrentPage Home currentRoute )
                ]
            , onClick (goTo Route.Home)
            ]
            [ i [ class "material-icons mdc-list-item__graphic" ]
                [ text "home" ]
            , span [ class "show-for-xlarge" ] [ text "Home" ]
            ]
        , a
            [ classList
                [ ( "mdc-list-item si-NavItem", True )
                , ( "mdc-list-item--selected si-NavItem-selected", isCurrentPage Projects currentRoute )
                ]
            , onClick (goTo Route.Projects)
            ]
            [ i [ class "material-icons mdc-list-item__graphic" ]
                [ text "work" ]
            , span [ class "show-for-xlarge" ] [ text "Projects" ]
            ]
        , a
            [ classList
                [ ( "mdc-list-item si-NavItem si-NavItem-selected", True )
                , ( "mdc-list-item--selected", isCurrentPage Tags currentRoute )
                ]
            , onClick (goTo Route.Tags)
            ]
            [ i [ class "material-icons mdc-list-item__graphic" ]
                [ text "label" ]
            , span [ class "show-for-xlarge" ] [ text "Tags" ]
            ]
        , a
            [ classList
                [ ( "mdc-list-item si-NavItem", True )
                , ( "mdc-list-item--selected si-NavItem-selected", isCurrentPage Reports currentRoute )
                ]
            , onClick (goTo Route.Reports)
            ]
            [ i [ class "material-icons mdc-list-item__graphic" ]
                [ text "view_list" ]
            , span [ class "show-for-xlarge" ] [ text "Reports" ]
            ]
        ]
