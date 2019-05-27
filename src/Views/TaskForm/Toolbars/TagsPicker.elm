module Views.TaskForm.Toolbars.TagsPicker exposing (render)

import Html exposing (Html, div, form, text)
import Html.Attributes exposing (class, id, placeholder, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Material
import Material.Icon as Icon
import Material.Options as Options exposing (cs)
import Material.Toolbar as Toolbar


endButton : String -> m -> m -> Html m
endButton needle clearNeedle hidePicker =
    if needle == "" then
        div [ onClick hidePicker, class "tf-TagsPicker_DoneLink" ] [ text "DONE" ]

    else
        Icon.view
            [ Toolbar.icon
            , Options.onClick clearNeedle
            , cs "tf-TagsPicker_DoneLink"
            ]
            "clear"


render :
    (Material.Msg m -> m)
    -> Material.Index
    -> Material.Model m
    -> m
    -> (String -> m)
    -> m
    -> String
    -> (String -> m)
    -> Html m
render lift idx model hidePicker setNeedle clearNeedle needle addTag =
    Toolbar.view lift
        idx
        model
        [ cs "tf-Picker_Toolbar mdc-toolbar tf-Picker_Toolbar-fixedsmall"
        ]
        [ Toolbar.row
            []
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ Icon.view
                    [ Toolbar.icon
                    , Options.onClick hidePicker
                    ]
                    "arrow_back"
                , form [ onSubmit (addTag needle), class "tf-Picker_TextField" ]
                    [ Html.input
                        [ onInput setNeedle
                        , value needle
                        , placeholder "Select Tag"
                        , id "tf-FilterProjectInput"
                        ]
                        []
                    ]
                ]
            , Toolbar.section
                [ Toolbar.alignEnd
                ]
                [ endButton needle clearNeedle hidePicker
                ]
            ]
        ]
