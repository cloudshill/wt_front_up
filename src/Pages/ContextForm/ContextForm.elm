module Pages.ContextForm.ContextForm exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , config
    , initEdit
    , initNew
    , smallTopbar
    , update
    , view
    )

import Data.Context exposing (Context)
import Data.Tag as Tag
import Data.User exposing (User)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Http
import Main.Route as Route exposing (Route)
import Main.Util exposing ((=>), pair)
import Material
import Material.Options as Options
import Material.Textfield as Textfield
import Pages.Shared.Form exposing (dirtyDialog, validationDialog)
import Requests.Context
import Validate exposing (Validator, ifBlank, ifEmptyList, validate)
import Views.Helpers exposing (ThreeColConfig, ThreeColMobileConfig, defaultThreeColConfig)
import Views.Model exposing (defaultModel)
import Views.TaskForm.SmallTopbar as SmallTopbar
import Views.TaskForm.TagsPicker as TagsPicker
import Views.TaskForm.TaskForm exposing (mediumTopbar)


type alias Model m =
    { mdc : Material.Model m
    , errors : List Error
    , isEditing : Bool
    , name : String
    , tags : List String
    , showTagsPicker : Bool
    , tagsPicker : TagsPicker.Model m
    , slug : Maybe String
    , showDirtyDialog : Bool
    , showValidationDialog : Bool
    , originalModel :
        { name : String
        , tags : List String
        }
    }


smallTopbar : (Msg m -> m) -> Model m -> Html m
smallTopbar lift model =
    if model.showTagsPicker then
        div [] []

    else
        SmallTopbar.render (Mdl >> lift)
            "cf-SmallTopbar"
            model.mdc
            "Enter Name..."
            model.name
            (lift << SetName)
            (lift Save)
            (lift CancelForm)


config : ThreeColConfig
config =
    let
        model =
            defaultModel

        c =
            defaultThreeColConfig model
    in
    { c
        | mobileConfig = ThreeColMobileConfig True True False
        , topbarTitle = "Contexts"
        , fabRoute = Route.NewContext
        , parentRoute = Route.Contexts
    }


initNew : List Tag.Tag -> Model m
initNew tags =
    { mdc = Material.defaultModel
    , errors = []
    , isEditing = False
    , name = ""
    , tags = []
    , showTagsPicker = False
    , tagsPicker = TagsPicker.initialModel tags
    , slug = Nothing
    , showDirtyDialog = False
    , showValidationDialog = False
    , originalModel =
        { name = ""
        , tags = []
        }
    }


initEdit : Context -> List Tag.Tag -> Model m
initEdit context tags =
    { mdc = Material.defaultModel
    , errors = []
    , isEditing = True
    , name = context.name
    , tags = context.tags
    , showTagsPicker = False
    , tagsPicker = TagsPicker.initialModel tags
    , slug = Just context.slug
    , showDirtyDialog = False
    , showValidationDialog = False
    , originalModel =
        { name = context.name
        , tags = context.tags
        }
    }


type Msg m
    = Mdl (Material.Msg m)
    | AddTag String
    | RemoveTag String
    | TagsPickerMsg (TagsPicker.Msg m)
    | ShowTagsPicker
    | HideTagsPicker
    | SetName String
    | GoTo Route
    | GoBack
    | CloseDialog
    | CancelForm
    | CreateCompleted (Result Http.Error Context)
    | UpdateCompleted (Result Http.Error Context)
    | Save


type ExternalMsg
    = NoOp
    | AppendContext Context
    | UpdateContext Context


type Field
    = Form
    | Name
    | Tags


type alias Error =
    ( Field, String )


modelValidator : Validator Error (Model m)
modelValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .name (Name => "Enter a name. A context must have a name.")
            , ifEmptyList .tags (Tags => "Select a tag. A context needs at least one tag")
            ]
        ]


update : User -> (Msg m -> m) -> Msg m -> Model m -> ( ( Model m, Cmd m ), ExternalMsg )
update user lift msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model => NoOp

        GoTo route ->
            model => Route.modifyUrl route => NoOp

        CloseDialog ->
            { model
                | showDirtyDialog = False
                , showValidationDialog = False
            }
                => Cmd.none
                => NoOp

        CancelForm ->
            let
                currentModel =
                    { name = model.name
                    , tags = model.tags
                    }
            in
            if currentModel == model.originalModel then
                model => Route.backOne => NoOp

            else
                { model | showDirtyDialog = True } => Cmd.none => NoOp

        GoBack ->
            ( model, Route.backOne ) => NoOp

        Save ->
            case validate modelValidator model of
                [] ->
                    if model.isEditing then
                        let
                            slug =
                                Maybe.withDefault "" model.slug
                        in
                        user.token
                            |> Requests.Context.update slug model
                            |> Http.send (lift << UpdateCompleted)
                            |> pair { model | errors = [] }
                            => NoOp

                    else
                        user.token
                            |> Requests.Context.create model
                            |> Http.send (lift << CreateCompleted)
                            |> pair { model | errors = [] }
                            => NoOp

                errors ->
                    { model | errors = errors, showValidationDialog = True } => Cmd.none => NoOp

        AddTag tag ->
            let
                tagsPicker =
                    model.tagsPicker
            in
            { model | tags = tag :: model.tags, tagsPicker = { tagsPicker | needle = "" } } => Cmd.none => NoOp

        RemoveTag tag ->
            { model | tags = List.filter (\e -> e /= tag) model.tags } => Cmd.none => NoOp

        TagsPickerMsg msg_ ->
            let
                ( newTagsPicker, effects ) =
                    TagsPicker.update (lift << TagsPickerMsg) msg_ model.tagsPicker
            in
            { model | tagsPicker = newTagsPicker } => effects => NoOp

        ShowTagsPicker ->
            { model | showTagsPicker = True } => Cmd.none => NoOp

        HideTagsPicker ->
            { model | showTagsPicker = False } => Cmd.none => NoOp

        SetName name ->
            { model | name = name } => Cmd.none => NoOp

        UpdateCompleted (Ok context) ->
            Route.Contexts
                |> Route.modifyUrl
                |> pair model
                => UpdateContext context

        UpdateCompleted (Err _) ->
            { model
                | errors =
                    model.errors
                        ++ [ Form
                                => "Server error while updating task"
                           ]
            }
                => Cmd.none
                => NoOp

        CreateCompleted (Ok context) ->
            Route.Contexts
                |> Route.modifyUrl
                |> pair model
                => AppendContext context

        CreateCompleted (Err _) ->
            { model
                | errors =
                    model.errors
                        ++ [ Form
                                => "Server error while creating task"
                           ]
            }
                => Cmd.none
                => NoOp


view : ThreeColConfig -> (Msg m -> m) -> Model m -> Html m
view config lift model =
    let
        form =
            if model.showTagsPicker then
                [ tagsPicker lift model ]

            else
                [ mediumTopbar (lift Save) (lift CancelForm) (Mdl >> lift) model.mdc
                , content lift model
                ]
    in
    div [ classList [ ( "g-ContentCol cell xlarge-3 medium-9 grid-y", True ), ( "show-for-xlarge", config.mobileConfig.hideThird ) ] ]
        [ div [ class "cell cell-block-y" ] form
        ]


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
    div [ class "cf-Form" ]
        [ div [ class "grid-x" ]
            [ div [ class "cell show-for-medium grid-x field" ]
                [ div [ class "cell" ]
                    [ Textfield.view (Mdl >> lift)
                        "cf-NameText"
                        model.mdc
                        [ Textfield.label "Name"
                        , Textfield.value model.name
                        , Options.onInput (SetName >> lift)
                        ]
                        []
                    ]
                ]
            , div [ class "cell grid-x align-middle field" ]
                [ div [ class "small-2 cell material-icons mdc-theme--text-icon-on-light" ] [ text "label" ]
                , div [ onClick (lift ShowTagsPicker), class "small-8 cell" ]
                    [ text (tagsToString model.tags)
                    ]
                , clearButtonList model.tags (lift ShowTagsPicker)
                ]
            ]
        , dirtyDialog (Mdl >> lift) model.mdc model.showDirtyDialog (lift CloseDialog) (lift GoBack) "context"
        , validationDialog (Mdl >> lift) model.mdc model.errors model.showValidationDialog (lift CloseDialog)
        ]


clearButtonList : List String -> m -> Html m
clearButtonList val clearMsg =
    if List.isEmpty val then
        div [] []

    else
        div [ onClick clearMsg, class "small-1 cell cancel-button mdc-theme--text-icon-on-light material-icons" ]
            [ text "clear"
            ]


tagsToString : List String -> String
tagsToString tags =
    if List.isEmpty tags then
        "Add Tags"

    else
        String.join ", " tags
