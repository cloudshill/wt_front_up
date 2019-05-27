module Pages.Shared.Fab exposing (Size(..), render)

import Html exposing (Html)
import List exposing (append)
import Material
import Material.Fab as Fab
import Material.Options as Options exposing (cs)


type Size
    = Normal
    | Mini


render :
    (Material.Msg m -> m)
    -> Material.Index
    -> Material.Model m
    -> Size
    -> String
    -> String
    -> m
    -> Html m
render lift idx model size fabClass icon linkTo =
    let
        fabSize =
            case size of
                Normal ->
                    []

                Mini ->
                    [ Fab.mini ]
    in
    Fab.view lift
        idx
        model
        (append
            fabSize
            [ Fab.ripple
            , cs fabClass
            , Options.onClick linkTo
            ]
        )
        icon
