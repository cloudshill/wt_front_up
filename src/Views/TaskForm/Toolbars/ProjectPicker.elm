module Views.TaskForm.Toolbars.ProjectPicker exposing (render)

import Html exposing (Html, form)
import Html.Attributes exposing (class, id, placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Material
import Material.Icon as Icon
import Material.Options as Options exposing (cs)
import Material.Toolbar as Toolbar


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
render lift idx model hidePicker setNeedle clearNeedle needle setProject =
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
                    [ Options.onClick hidePicker
                    , Toolbar.icon
                    ]
                    "arrow_back"
                , form [ onSubmit (setProject needle), class "tf-Picker_TextField" ]
                    [ Html.input
                        [ onInput setNeedle
                        , value needle
                        , id "tf-FilterProjectInput"
                        , placeholder "Select Project"
                        ]
                        []
                    ]
                ]
            , Toolbar.section
                [ Toolbar.alignEnd
                ]
                [ Icon.view
                    [ Options.onClick hidePicker
                    , cs "done"
                    , Toolbar.icon
                    ]
                    "clear"
                ]
            ]
        ]
