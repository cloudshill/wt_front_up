module Views.TaskForm.DateField exposing (dialog, view)

import Data.Task
    exposing
        ( DateField(..)
        , Period(..)
        , Priority(..)
        )
import Date exposing (Date)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as DateFormat
import DatePicker
import Html exposing (Html, div, label, text)
import Html.Attributes exposing (attribute, class, classList, for)
import Html.Events exposing (onClick)
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Options as Options exposing (when)
import Views.TaskForm.Helpers exposing (clearButton)
import Views.TaskForm.Model exposing (Model)
import Views.TaskForm.Msg exposing (ExternalMsg(..), Msg(..))


datePlaceholderColorClass : Maybe Date -> String
datePlaceholderColorClass dateArg =
    if dateArg == Nothing then
        "tf-Form_Placeholder"

    else
        ""


dateToString : Maybe Date -> String
dateToString dateArg =
    case dateArg of
        Nothing ->
            "Add Date"

        Just date ->
            DateFormat.format config "%a, %b %e, %Y" date


view : (Msg m -> m) -> Maybe Date -> DateField -> String -> Bool -> m -> Html m
view lift date dateField icon showMoreDates showDialog =
    let
        hideDate =
            dateField /= Due && not showMoreDates
    in
    div [ classList [ ( "tf-Form_DateField cell grid-x", True ), ( "tf-Form_DateField-hide", hideDate ) ] ]
        [ div [ class "small-2 cell material-icons mdc-theme--text-icon-on-light" ] [ text icon ]
        , div [ class "small-8 cell" ]
            [ div [ class (datePlaceholderColorClass date ++ " mdc-text-field mdc-text-field--upgraded") ]
                [ div
                    [ onClick showDialog
                    , class "tf-Form_Link"
                    ]
                    [ text (dateToString date) ]
                , label [ class "mdc-floating-label mdc-floating-label--float-above", for "fouc" ]
                    [ text (toString dateField ++ ":          ") ]
                , div
                    [ class "mdc-text-field__bottom-line"
                    , attribute "style" "transform-origin: 78px center"
                    ]
                    []
                ]
            ]
        , clearButton date (lift (Reset dateField))
        ]


dialog : (Msg m -> m) -> Model m -> DateField -> Int -> Bool -> Html m
dialog lift model dateField idx showDialog =
    let
        idx2 =
            idx + 1
    in
    Dialog.view (Mdl >> lift)
        (toString idx)
        model.mdc
        [ Options.cs "tf-DatePicker_Dialog"
        , Dialog.open |> when showDialog
        , Dialog.onClose (lift CloseDialog)
        ]
        [ Dialog.surface [ Options.cs "tf-DatePicker_DialogSurface" ]
            [ Dialog.body [ Options.cs "tf-DatePicker_DialogBody" ]
                [ Html.map
                    (lift << DatePickerMsg)
                    (DatePicker.view model.datePicker)
                ]
            , Dialog.footer []
                [ Button.view (Mdl >> lift)
                    (toString idx)
                    model.mdc
                    [ Options.onClick (lift CloseDialog) ]
                    [ text "clear" ]
                , Button.view (Mdl >> lift)
                    (toString idx2)
                    model.mdc
                    [ Options.onClick (lift (DateSelected dateField))
                    ]
                    [ text "Ok" ]
                ]
            ]
        , Dialog.backdrop [] []
        ]
