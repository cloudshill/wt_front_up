module Views.LoadingBar.LoadingBar exposing (render)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.Lazy exposing (lazy2)
import Main.Util as Util


render : Bool -> String -> Html msg
render isLoading additionalClasses =
    div [ class ("lb-Container " ++ additionalClasses) ]
        [ lazy2 Util.viewIf isLoading (div [ class "lb-Bar" ] [])
        ]
