module Pages.Shared.Form exposing (dirtyDialog, validationDialog)

import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Material
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Options as Options exposing (cs, when)


dirtyDialog : (Material.Msg m -> m) -> Material.Model m -> Bool -> m -> m -> String -> Html m
dirtyDialog lift model showDialog closeDialog goBack object =
    Dialog.view lift
        "sf-DirtyDialog"
        model
        [ Dialog.open |> when showDialog
        , Dialog.onClose closeDialog
        ]
        [ Dialog.surface []
            [ Dialog.header [] []
            , Dialog.body [ cs "grid-x" ]
                [ div [ class "cell" ]
                    [ text ("Are you sure you want to discard this " ++ object ++ "?")
                    ]
                ]
            , Dialog.footer []
                [ Button.view lift
                    "sf-DirtyDialog__DiscardButton"
                    model
                    [ Options.onClick goBack ]
                    [ text "Discard" ]
                , Button.view lift
                    "sf-DirtyDialog__KeepEditingButton"
                    model
                    [ Options.onClick closeDialog ]
                    [ text "Keep Editing" ]
                ]
            ]
        , Dialog.backdrop [] []
        ]


viewErrors : List ( a, String ) -> Html msg
viewErrors errors =
    errors
        |> List.map (\( _, error ) -> p [] [ text error ])
        |> div [ class "cell" ]


validationDialog : (Material.Msg m -> m) -> Material.Model m -> List ( a, String ) -> Bool -> m -> Html m
validationDialog lift model errors showDialog closeDialog =
    Dialog.view lift
        "sf-ValidationDialog"
        model
        [ Dialog.open |> when showDialog
        , Dialog.onClose closeDialog
        ]
        [ Dialog.surface []
            [ Dialog.header [] []
            , Dialog.body [ cs "grid-x" ]
                [ viewErrors errors
                ]
            , Dialog.footer []
                [ Button.view lift
                    "sf-ValidationDialog__Button"
                    model
                    [ Options.onClick closeDialog ]
                    [ text "Ok" ]
                ]
            ]
        , Dialog.backdrop [] []
        ]
