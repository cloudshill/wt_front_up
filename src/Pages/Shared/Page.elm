module Pages.Shared.Page exposing
    ( CurrentPage(..)
    , FixedAdjust(..)
    , blankState
    , detailFormFrame
    , frame
    , initBlocksSpinner
    , isCurrentPage
    )

import Html exposing (Attribute, Html, div, h3, img, text)
import Html.Attributes exposing (class, src)
import Main.Route as Route
import Material.Button as Button
import Material.Options exposing (cs, onClick)


type FixedAdjust
    = FixedAdjustOn
    | FixedAdjustOff


type CurrentPage
    = Home
    | Projects
    | Tags
    | Contexts
    | Reports
    | Other


isCurrentPage : CurrentPage -> Route.Route -> Bool
isCurrentPage page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Projects, Route.Projects ) ->
            True

        ( Tags, Route.Tags ) ->
            True

        ( Contexts, Route.Contexts ) ->
            True

        ( Reports, Route.Reports ) ->
            True

        _ ->
            False


blankState : String -> String -> Html m -> Html m
blankState item image createTaskButton =
    div [ class "bs-BlankSlate grid-x align-center-middle" ]
        [ div [ class "grid-x cell align-center-middle" ]
            [ div [ class "cell xlarge-6" ]
                [ h3 [ class "bs-Header mdc-typography--headline3" ] [ text ("You have no " ++ item ++ ".") ]
                ]
            ]
        , div [ class "grid-x cell" ]
            [ div [ class "bs-Container cell xlarge-12" ]
                [ img [ class "bs-Image", src ("/images/iconochki/256x256/" ++ image ++ ".png") ] []
                ]
            ]
        , div [ class "grid-x cell" ]
            [ div [ class "cell bs-Container xlarge-12" ]
                [ createTaskButton
                ]
            ]
        ]


initBlocksSpinner : Html m
initBlocksSpinner =
    div [ class "bs-SpinnerContainer" ]
        [ img [ class "", src "/images/blocks_spinner/blocks.svg" ] []
        ]


detailFormFrame :
    List (Html msg)
    -> Maybe (Html msg)
    -> List (Html msg)
    -> FixedAdjust
    -> Html msg
    -> Html msg
detailFormFrame topbars maybeBottombar fabAndSnack fixedAdjust content =
    let
        bottombar =
            case maybeBottombar of
                Nothing ->
                    div [] []

                Just justBottombar ->
                    justBottombar

        mainContent =
            List.append fabAndSnack [ content, bottombar ]
    in
    div [ adjustClass fixedAdjust ]
        (List.append topbars
            mainContent
        )


frame :
    List (Html msg)
    -> Maybe (Html msg)
    -> Html msg
    -> FixedAdjust
    -> Html msg
    -> Html msg
frame topbars maybeBottombar fab fixedAdjust content =
    let
        bottombar =
            case maybeBottombar of
                Nothing ->
                    div [] []

                Just justBottombar ->
                    justBottombar

        main =
            List.append topbars [ content, bottombar ]
    in
    div [ adjustClass fixedAdjust ]
        (List.append
            [ fab ]
            main
        )


adjustClass : FixedAdjust -> Attribute msg
adjustClass fixedAdjust =
    case fixedAdjust of
        FixedAdjustOn ->
            class "g-Frame-fixedadjust"

        FixedAdjustOff ->
            class ""
