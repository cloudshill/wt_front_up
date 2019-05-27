module Views.Layouts.Welcome exposing (view)

import Html exposing (Html, div, main_)
import Html.Attributes exposing (class)


view : Html m -> Html m
view content =
    main_ [ class "main grid-y cell-block-container" ]
        [ div [ class "grid-x grid-padding-x cell main-content" ]
            [ emptyCol
            , content
            , emptyCol
            ]
        ]


emptyCol : Html m
emptyCol =
    div [ class "cell show-for-medium medium-2" ] []
