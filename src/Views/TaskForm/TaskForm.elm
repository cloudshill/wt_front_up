module Views.TaskForm.TaskForm exposing
    ( mediumTopbar
    , smallTopbar
    , subscriptions
    , view
    , viewOneCol
    )

import Data.Task
    exposing
        ( DateField(..)
        , Period(..)
        , Priority(..)
        )
import Html exposing (Html, div, hr, span, text)
import Html.Attributes exposing (class, classList, for, id)
import Html.Events exposing (onClick)
import Material
import Material.Button as Button
import Material.Dialog as Dialog
import Material.Elevation as Elevation
import Material.Icon as Icon
import Material.Options as Options exposing (cs, css, when)
import Material.RadioButton as RadioButton
import Material.Textfield as Textfield
import Material.Theme as Theme
import Material.Toolbar as Toolbar
import Pages.Shared.Form exposing (dirtyDialog, validationDialog)
import Views.Helpers exposing (ThreeColConfig)
import Views.TaskForm.DateField as DateField
import Views.TaskForm.Helpers exposing (clearButton)
import Views.TaskForm.Model exposing (Model)
import Views.TaskForm.Msg exposing (ExternalMsg(..), Msg(..))
import Views.TaskForm.ProjectPicker as ProjectPicker
import Views.TaskForm.SmallTopbar as SmallTopbar
import Views.TaskForm.TagsPicker as TagsPicker


mediumTopbar : m -> m -> (Material.Msg m -> m) -> Material.Model m -> Html m
mediumTopbar save cancel mdcMsg mdcModel =
    Toolbar.view mdcMsg
        "tf-Topbar"
        mdcModel
        [ Theme.secondaryBg, Elevation.z2, Options.cs "show-for-medium" ]
        [ Toolbar.row []
            [ Toolbar.section
                [ Toolbar.alignStart
                ]
                [ cancelButton cancel
                ]
            , Toolbar.section
                [ Toolbar.alignEnd
                ]
                [ saveButton save mdcMsg mdcModel
                ]
            ]
        ]


saveButton : m -> (Material.Msg m -> m) -> Material.Model m -> Html m
saveButton save mdcMsg mdcModel =
    Button.view
        mdcMsg
        "tf-SaveButton"
        mdcModel
        [ Options.onClick save
        , css "color" "#fff"
        , cs "te-save-button"
        ]
        [ text "Save" ]


cancelButton : m -> Html m
cancelButton cancel =
    Icon.view [ Toolbar.icon, Options.onClick cancel ] "close"


viewOneCol : (Msg m -> m) -> Model m -> Html m
viewOneCol lift model =
    let
        form =
            if model.showProjectPicker then
                div [ class "cell auto grid-y tf-Picker" ]
                    [ div [ class "medium-12 cell cell-block-y" ]
                        [ projectPicker lift model ]
                    ]

            else if model.showTagsPicker then
                div [ class "cell auto grid-y tf-Picker" ]
                    [ div [ class "medium-12 cell cell-block-y" ]
                        [ tagsPicker lift model ]
                    ]

            else
                div [ class "g-ContentCol cell auto grid-y " ]
                    [ div [ class "medium-12 cell cell-block-y" ]
                        [ mediumTopbar (lift Save) (lift CancelForm) (Mdl >> lift) model.mdc
                        , content lift model
                        ]
                    ]
    in
    form


view : (Msg m -> m) -> ThreeColConfig -> Model m -> Html m
view lift config model =
    let
        form =
            if model.showProjectPicker then
                [ div []
                    []
                , div [ class "xlarge-12 cell cell-block-y tf-Picker" ]
                    [ projectPicker lift model ]
                ]

            else if model.showTagsPicker then
                [ div []
                    []
                , div [ class "xlarge-12 cell cell-block-y tf-Picker" ]
                    [ tagsPicker lift model ]
                ]

            else
                [ div []
                    [ mediumTopbar (lift Save) (lift CancelForm) (Mdl >> lift) model.mdc ]
                , div [ class "xlarge-12 cell cell-block-y" ]
                    [ content lift model
                    , div [ class "show-for-xlarge tf-BottomSpacer" ] []
                    ]
                ]
    in
    div [ classList [ ( "tf-FormContainer g-ContentCol cell medium-9 xlarge-3 grid-y", True ), ( "show-for-xlarge", config.mobileConfig.hideThird ) ] ]
        form


smallTopbar : (Msg m -> m) -> String -> Model m -> Html m
smallTopbar lift description model =
    if model.showProjectPicker then
        div [] []

    else if model.showTagsPicker then
        div [] []

    else
        SmallTopbar.render (Mdl >> lift)
            "tf-Appbar"
            model.mdc
            "Enter Description..."
            description
            (lift << SetDescription)
            (lift Save)
            (lift CancelForm)


projectPicker : (Msg m -> m) -> Model m -> Html m
projectPicker lift model =
    ProjectPicker.view
        (lift << ProjectPickerMsg)
        model.projectPicker
        (lift HideProjectPicker)
        (lift << SetProject)


tagsPicker : (Msg m -> m) -> Model m -> Html m
tagsPicker lift model =
    TagsPicker.view (lift << TagsPickerMsg)
        model.tagsPicker
        (lift HideTagsPicker)
        (lift << AddTag)
        (lift << RemoveTag)
        model.tags


content : (Msg m -> m) -> Model m -> Html m
content lift model =
    div [ class "tf-Form" ]
        [ div [ class "grid-x" ]
            [ div [ class "cell show-for-medium grid-x tf-Form_Field" ]
                [ div [ class "cell" ]
                    [ Textfield.view (Mdl >> lift)
                        "tf-DescriptionText"
                        model.mdc
                        [ Textfield.label "Description"
                        , Textfield.value model.description
                        , Textfield.textarea
                        , Options.onInput (SetDescription >> lift)
                        , Options.cs "tf-DescriptionText"
                        ]
                        []
                    ]
                ]
            , div [ class "cell show-for-medium" ]
                [ hr [ class "tf-Form_Divider" ] []
                ]
            , div [ class "cell grid-x tf-Form_Field" ]
                [ div [ class "small-2 cell material-icons mdc-theme--text-icon-on-light" ] [ text "work" ]
                , div
                    [ onClick (lift ShowProjectPicker)
                    , class ("small-8 cell tf-Form_Link " ++ textColorClass model.project)
                    ]
                    [ text (projectToString model.project)
                    ]
                , clearButton model.project (lift ResetProject)
                ]
            , div [ class "cell" ]
                [ hr [ class "tf-Form_Divider" ] []
                ]
            , div [ class "cell grid-x align-middle tf-Form_Field" ]
                [ div [ class "small-2 cell material-icons mdc-theme--text-icon-on-light" ] [ text "label" ]
                , div
                    [ onClick (lift ShowTagsPicker)
                    , class ("tf-Form_Link small-8 cell " ++ tagsColorClass model.tags)
                    ]
                    [ text (tagsToString model.tags)
                    ]
                , clearButtonList model.tags (lift ResetTags)
                ]
            , div [ class "cell" ]
                [ hr [ class "tf-Form_Divider" ] []
                ]
            , div [ class "cell grid-x tf-Form_Field" ]
                [ DateField.view lift model.due Due "access_time" model.showMoreDates (lift ShowDueDialog)
                , moreDates lift model.showMoreDates
                , DateField.view lift model.start Start "" model.showMoreDates (lift ShowStartDialog)
                , DateField.view lift model.end End "" model.showMoreDates (lift ShowEndDialog)
                , DateField.view lift model.scheduled Scheduled "" model.showMoreDates (lift ShowScheduledDialog)
                , DateField.view lift model.until Until "" model.showMoreDates (lift ShowUntilDialog)
                , DateField.view lift model.wait Wait "" model.showMoreDates (lift ShowWaitDialog)
                ]
            , div [ class "cell" ]
                [ hr [ class "tf-Form_Divider" ] []
                ]
            , div [ class "cell grid-x tf-Form_Field" ]
                [ div [ class "small-2 cell material-icons mdc-theme--text-icon-on-light" ] [ text "autorenew" ]
                , div
                    [ class ("tf-Form_Link small-8 cell " ++ textColorClass model.recur)
                    , onClick (lift ShowRecurDialog)
                    ]
                    [ text (periodToString model.recur)
                    ]
                , clearButton model.recur (lift ResetRecur)
                ]
            , div [ class "cell" ]
                [ hr [ class "tf-Form_Divider" ] []
                ]
            , div [ class "cell grid-x align-middle tf-Form_Field" ]
                [ div [ class "small-2 cell material-icons mdc-theme--text-icon-on-light" ] [ text "flag" ]
                , div
                    [ onClick (lift ShowPriorityDialog)
                    , class ("tf-Form_Link small-8 cell " ++ textColorClass model.priority)
                    ]
                    [ text (priorityToString model.priority)
                    ]
                , clearButton model.priority (lift ResetPriority)
                ]
            ]
        , recurDialog lift model
        , priorityDialog lift model
        , DateField.dialog lift model Due 22 model.showDueDialog
        , DateField.dialog lift model Start 24 model.showStartDialog
        , DateField.dialog lift model End 26 model.showEndDialog
        , DateField.dialog lift model Scheduled 28 model.showScheduledDialog
        , DateField.dialog lift model Until 30 model.showUntilDialog
        , DateField.dialog lift model Wait 32 model.showWaitDialog
        , dirtyDialog (Mdl >> lift) model.mdc model.showDirtyDialog (lift CloseDialog) (lift GoBack) "task"
        , validationDialog (Mdl >> lift) model.mdc model.errors model.showValidationDialog (lift CloseDialog)
        ]


periodToString : Maybe Period -> String
periodToString periodArg =
    case periodArg of
        Nothing ->
            "Does Not Repeat"

        Just period ->
            toString period


textColorClass : Maybe a -> String
textColorClass arg =
    if arg == Nothing then
        "tf-Form_Placeholder"

    else
        ""


isPrioritySelected : Model m -> Priority -> Bool
isPrioritySelected model priority =
    let
        priorityModel =
            model.radioPriority
    in
    case priorityModel of
        Nothing ->
            False

        Just priorityModel ->
            priorityModel == priority


isPeriodSelected : Model m -> Period -> Bool
isPeriodSelected model period =
    let
        recur =
            model.radioRecur
    in
    case recur of
        Nothing ->
            False

        Just recur ->
            recur == period


priorityDialog : (Msg m -> m) -> Model m -> Html m
priorityDialog lift model =
    Dialog.view (Mdl >> lift)
        "tf-PriorityDialog"
        model.mdc
        [ Dialog.open |> when model.showPriorityDialog
        , Dialog.onClose (lift CloseDialog)
        , Options.cs "tf-PriorityDialog"
        ]
        [ Dialog.surface []
            [ Dialog.header []
                [ Options.styled Html.h2 [ Dialog.title ] [ text "Set Priority" ]
                ]
            , Dialog.body [ cs "grid-x" ]
                [ div [ class "cell mdc-form-field" ]
                    [ RadioButton.view (Mdl >> lift)
                        "tf-PriorityDialog_RadioButton-low"
                        model.mdc
                        [ Options.onClick (lift (SetRadioPriority (Just Low)))
                        , Options.attribute (Html.Attributes.id "priority-low")
                        , RadioButton.selected
                            |> when (isPrioritySelected model Low)
                        ]
                        []
                    , Html.label
                        [ onClick (lift (SetRadioPriority (Just Low)))
                        , for "priority-low"
                        ]
                        [ text "Low" ]
                    ]
                , div [ class "cell mdc-form-field" ]
                    [ RadioButton.view (Mdl >> lift)
                        "tf-PriorityDialog_RadioButton-medium"
                        model.mdc
                        [ Options.onClick (lift (SetRadioPriority (Just Medium)))
                        , Options.attribute (Html.Attributes.id "priority-medium")
                        , RadioButton.selected
                            |> when (isPrioritySelected model Medium)
                        ]
                        []
                    , Html.label
                        [ onClick (lift (SetRadioPriority (Just Medium)))
                        , for "priority-medium"
                        ]
                        [ text "Medium" ]
                    ]
                , div [ class "cell mdc-form-field" ]
                    [ RadioButton.view (Mdl >> lift)
                        "tf-PriorityDialog_RadioButton-high"
                        model.mdc
                        [ Options.onClick (lift (SetRadioPriority (Just High)))
                        , Options.attribute (Html.Attributes.id "priority-high")
                        , RadioButton.selected
                            |> when (isPrioritySelected model High)
                        ]
                        []
                    , Html.label
                        [ onClick (lift (SetRadioPriority (Just High)))
                        , for "priority-high"
                        ]
                        [ text "High" ]
                    ]
                ]
            , Dialog.footer []
                [ Button.view (Mdl >> lift)
                    "tf-PriorityDialog_Button-cancel"
                    model.mdc
                    [ Options.onClick (lift CancelPriorityDialog)
                    ]
                    [ text "Cancel" ]
                , Button.view (Mdl >> lift)
                    "tf-PriorityDialog_Button-ok"
                    model.mdc
                    [ Options.onClick (lift SetPriority)
                    ]
                    [ text "Ok" ]
                ]
            ]
        , Dialog.backdrop [] []
        ]


recurDialog : (Msg m -> m) -> Model m -> Html m
recurDialog lift model =
    Dialog.view (Mdl >> lift)
        "tf-RecurDialog"
        model.mdc
        [ Dialog.open |> when model.showRecurDialog
        , Dialog.onClose (lift CloseDialog)
        , Options.cs "tf-RecurDialog"
        ]
        [ Dialog.surface []
            [ Dialog.header []
                [ Options.styled Html.h2 [ Dialog.title ] [ text "Set Recurrence Interval" ]
                ]
            , Dialog.body [ cs "grid-x" ]
                [ recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-annual" Annual
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-biannual" Biannual
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-biweekly" Biweekly
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-biyearly" Biyearly
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-daily" Daily
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-day" Day
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-fortnight" Fortnight
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-monthly" Monthly
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-quarterly" Quarterly
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-semiannual" Semiannual
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-weekdays" Weekdays
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-weekly" Weekly
                , recurrenceRadioButton lift model "tf-RecurDialog_RadioButton-yearly" Yearly
                ]
            , Dialog.footer []
                [ Button.view (Mdl >> lift)
                    "tf-RecurDialog_Button-cancel"
                    model.mdc
                    [ Options.onClick (lift CancelRecurDialog)
                    ]
                    [ text "cancel" ]
                , Button.view (Mdl >> lift)
                    "tf-RecurDialog_Button-ok"
                    model.mdc
                    [ Options.onClick (lift SetRecur)
                    ]
                    [ text "Ok" ]
                ]
            ]
        , Dialog.backdrop [] []
        ]


recurrenceRadioButton : (Msg m -> m) -> Model m -> Material.Index -> Period -> Html m
recurrenceRadioButton lift model idx period =
    let
        radioId =
            String.join "-" [ "recurrence", toString period ]
    in
    div [ class "cell mdc-form-field" ]
        [ RadioButton.view (Mdl >> lift)
            idx
            model.mdc
            [ Options.onClick (lift (SetRadioRecur (Just period)))
            , Options.attribute (id radioId)
            , RadioButton.selected |> when (isPeriodSelected model period)
            ]
            []
        , Html.label
            [ onClick (lift (SetRadioRecur (Just period)))
            , for radioId
            ]
            [ text (toString period) ]
        ]


priorityToString : Maybe Priority -> String
priorityToString priorityArg =
    case priorityArg of
        Nothing ->
            "Add Priority"

        Just priority ->
            toString priority


projectToString : Maybe String -> String
projectToString projectArg =
    case projectArg of
        Nothing ->
            "Add Project"

        Just project ->
            project


tagsToString : List String -> String
tagsToString tags =
    if List.isEmpty tags then
        "Add Tags"

    else
        String.join ", " tags


clearButtonList : List String -> m -> Html m
clearButtonList val clearMsg =
    if List.isEmpty val then
        div [] []

    else
        div
            [ onClick clearMsg
            , class "small-1 cell cancel-button mdc-theme--text-icon-on-light material-icons"
            ]
            [ text "clear"
            ]


moreDates : (Msg m -> m) -> Bool -> Html m
moreDates lift showMoreDates =
    if showMoreDates then
        span [] []

    else
        div
            [ onClick (lift ShowMoreDates)
            , class "cell grid-x tf-Form_Placeholder tf-Form_MoreDatesLink"
            ]
            [ div [ class "small-8 cell" ] []
            , div [ class "small-4 cell" ] [ text "More Dates" ]
            ]


tagsColorClass : List a -> String
tagsColorClass list =
    if List.isEmpty list then
        "tf-Form_Placeholder"

    else
        ""


subscriptions : (Msg m -> m) -> Model m -> Sub m
subscriptions lift model =
    Sub.batch
        [ Material.subscriptions (Mdl >> lift) model
        ]
