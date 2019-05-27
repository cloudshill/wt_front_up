module Pages.Shared.Spinner exposing (spinner)

import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class)


spinner : Html msg
spinner =
    div [ class "spinner" ]
        [ i [ class "fa fa-spin fa-cog fa-3x" ] []
        , div [] [ text "Loading..." ]
        ]
