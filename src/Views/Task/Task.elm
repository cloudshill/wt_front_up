module Views.Task.Task exposing (field, view)

import Data.Task exposing (Task)
import Dict
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Material.Options exposing (cs, styled)
import Material.Typography as Typography


view : Task -> Html m
view task =
    let
        fieldNames =
            Dict.keys task.fields

        fieldValues =
            Dict.values task.fields
    in
    div [ class "grid-container tk-Content" ]
        (List.map2 (\n v -> field n v) fieldNames fieldValues)


field : String -> String -> Html m
field name value =
    styled div
        [ Typography.body1, cs "grid-x grid-margin-x tk-FieldGrid" ]
        [ div [ class "tk-FieldLabel small-3 cell" ]
            [ text name
            ]
        , div [ class "small-9 cell" ]
            [ text value
            ]
        ]
