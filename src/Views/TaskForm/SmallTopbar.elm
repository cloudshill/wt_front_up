module Views.TaskForm.SmallTopbar exposing (render)

import Html exposing (Html, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events as Html
import Material
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options exposing (cs, css)
import Material.Textfield as Textfield
import Material.Toolbar as Toolbar


render :
    (Material.Msg m -> m)
    -> Material.Index
    -> Material.Model m
    -> String
    -> String
    -> (String -> m)
    -> m
    -> m
    -> Html m
render lift idx model placeholderText valueText setDescription saveTask cancelForm =
    Toolbar.view lift
        idx
        model
        [ Toolbar.flexible
        , cs "tf-SmallTopbar"
        ]
        [ Toolbar.row []
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ Icon.view [ Toolbar.icon, Options.onClick cancelForm ] "clear"
                ]
            , Toolbar.section
                [ Toolbar.alignEnd
                ]
                [ Button.view
                    lift
                    "tf-SmallTopbar_SaveButton"
                    model
                    [ Options.onClick saveTask
                    , cs "tf-SmallTopbar_SaveButton"
                    ]
                    [ text "Save" ]
                ]
            ]
        , Toolbar.row [ css "height" "25" ]
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ input
                    [ placeholder placeholderText
                    , value valueText
                    , Html.onInput setDescription
                    , class "tf-SmallTopbar_DescriptionInput"
                    ]
                    []
                ]
            ]
        ]
