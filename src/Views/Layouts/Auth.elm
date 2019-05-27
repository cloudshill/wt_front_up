module Views.Layouts.Auth exposing (view)

import Html exposing (Html, div, main_)
import Html.Attributes exposing (class)


view : Html m -> Html m -> Html m
view topbar mainCol =
    div []
        [ topbar
        , content mainCol
        ]


content : Html m -> Html m
content mainCol =
    main_ [ class "main grid-y g-GridFrame cell-block-container mdc-toolbar-fixed-adjust" ]
        [ div [ class "grid-x grid-padding-x center-middle cell main-content" ]
            [ emptySideCol
            , mainCol
            , emptySideCol
            ]
        ]


emptySideCol : Html m
emptySideCol =
    div [ class "cell show-for-medium xlarge-4 medium-2 grid-y" ] []
