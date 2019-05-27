module Views.ContentColTopbar.ContentColTopbar exposing (render)

import Html exposing (Html, span)
import Material
import Material.Elevation as Elevation
import Material.Options as Options
import Material.Toolbar as Toolbar


optionalEndSection : List (Html m) -> Html m
optionalEndSection endSection =
    if List.isEmpty endSection then
        span [] []

    else
        Toolbar.section
            [ Toolbar.alignEnd
            ]
            endSection


render :
    (Material.Msg m -> m)
    -> String
    -> Material.Model m
    -> List (Html m)
    -> List (Html m)
    -> Html m
render mdcMessage identifier mdc startSection endSection =
    Toolbar.view mdcMessage
        identifier
        mdc
        [ Elevation.z2, Options.cs "cc-Topbar show-for-medium" ]
        [ Toolbar.row []
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                startSection
            , optionalEndSection endSection
            ]
        ]
