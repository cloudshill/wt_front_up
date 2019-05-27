module Views.Errored.Errored exposing (PageLoadError, pageLoadError, view)

import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)
import Pages.Shared.Page exposing (CurrentPage)


type PageLoadError
    = PageLoadError Model


type alias Model =
    { errorMessage : String
    , activePage : CurrentPage
    }


pageLoadError : CurrentPage -> String -> PageLoadError
pageLoadError activePage errorMessage =
    PageLoadError { errorMessage = errorMessage, activePage = activePage }


view : PageLoadError -> m -> Html m
view (PageLoadError model) refreshData =
    div [ class "g-ContentCol cell auto" ]
        [ h1 [] [ text "Apologies, an unexpected error just occurred." ]
        , p [] [ text "What happens now?" ]
        , p [] [ text "Visit the WingTask forum and open a new topic in the Support section." ]
        , a
            [ href "https://forum.wingtask.com"
            , class "mdc-js-button mdc-button"
            ]
            [ text "Visit WingTask Forum" ]
        , p [] [ text "It's possible the error is a slight momentary hiccup and you might try the operation again. The best way to do do this is by refreshing the data which is kind of a reset button." ]
        , a
            [ onClick refreshData
            , class "mdc-js-button mdc-button mdc-button--raised mdc-theme--secondary-bg ee-RefreshButton"
            ]
            [ text "Refresh Data" ]
        , p []
            [ text "If you want more direct help email me at "
            , a [ href "mailto:tim@wingtask.com" ] [ text "tim@wingtask.com" ]
            , text " and I'll work with you to get this error fixed."
            ]
        ]
