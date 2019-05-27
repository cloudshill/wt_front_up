module Views.TaskForm.Helpers exposing (clearButton)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


clearButton : Maybe a -> m -> Html m
clearButton val clearMsg =
    case val of
        Nothing ->
            div [] []

        Just _ ->
            div
                [ onClick clearMsg
                , class "tf-ClearButton small-1 cell mdc-theme--text-icon-on-light material-icons"
                ]
                [ text "clear"
                ]
