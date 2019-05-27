module Views.Auth.ChangePassword exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , init
    , initialModel
    , topbar
    , update
    , view
    )

import Data.Session exposing (Session)
import Data.User exposing (PasswordResetToken, User)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, string)
import Json.Decode.Pipeline as Pipeline exposing (decode)
import Main.Route as Route
import Main.Util exposing ((=>))
import Material exposing (Msg)
import Material.Button as Button
import Material.Options as Options
import Material.Textfield as Textfield
import Material.Textfield.HelperText as TextfieldHelper
import Pages.Shared.Page as Page
import Pages.Shared.Toolbars.Auth as Toolbar
import Pages.Shared.Validation
    exposing
        ( (>=>)
        , (|:)
        , Event(..)
        , Field
        , Validity(..)
        , extractError
        , field
        , isAtLeast8Chars
        , isNotEmpty
        , setError
        , validate
        , validity
        )
import Requests.User
import Task exposing (Task)
import Views.Errored.Errored exposing (PageLoadError, pageLoadError)


type alias Model m =
    { mdc : Material.Model m
    , password : Field String String
    , base : Field String String
    , user : Maybe User
    , isLoading : Bool
    }


initialModel : Model m
initialModel =
    { mdc = Material.defaultModel
    , password = field ""
    , base = field ""
    , user = Nothing
    , isLoading = False
    }


init : PasswordResetToken -> Task PageLoadError (Model m)
init passwordResetToken =
    Requests.User.loginWithResetToken passwordResetToken
        |> Http.toTask
        |> Task.mapError
            (\_ ->
                pageLoadError Page.Other "Reset token not found or expired"
            )
        |> Task.map
            (\model ->
                { base = field ""
                , mdc = Material.defaultModel
                , user = Just model
                , password = field ""
                , isLoading = False
                }
            )


topbar : (Msg m -> m) -> Model m -> Bool -> Html m
topbar lift model isLoading =
    Toolbar.render (Mdl >> lift)
        "cp-Toolbar"
        model.mdc
        "Change Password"
        isLoading


view : (Msg m -> m) -> Model m -> Html m
view lift model =
    div [ class "cell auto au-ChangePasswordForm g-ContentCol" ]
        [ div [ class "grid-x au-ChangePassword_Password au-Input" ]
            [ div [ class "cell" ]
                [ Textfield.view (Mdl >> lift)
                    "au-PasswordText"
                    model.mdc
                    ([ Textfield.label "Password *"
                     , Textfield.password
                     , Options.onInput (SetPassword >> lift)
                     , Options.onBlur (lift BlurPassword)
                     ]
                        ++ errorClass model.password
                    )
                    []
                , helperText model.password ""
                ]
            ]
        , div [ class "grid-x au-SubmitButton" ]
            [ div [ class "cell" ]
                [ Button.view (Mdl >> lift)
                    "au-SubmitButton"
                    model.mdc
                    [ Button.ripple
                    , Options.onClick (lift SubmitForm)
                    , Button.raised
                    ]
                    [ text "Save New Password"
                    ]
                ]
            ]
        , div [ class "grid-x" ]
            [ div [ class "cell" ]
                []
            ]
        ]



-- UPDATE --


type Msg m
    = Mdl (Material.Msg m)
    | SubmitForm
    | SetPassword String
    | BlurPassword
    | ChangePasswordCompleted (Result Http.Error User)


type ExternalMsg
    = NoOp


update :
    Session
    -> (Msg m -> m)
    -> Msg m
    -> Model m
    -> ( ( Model m, Cmd m ), ExternalMsg )
update session lift msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model => NoOp

        SubmitForm ->
            model |> validateModel |> submitIfValid session lift => NoOp

        SetPassword p ->
            { model
                | password =
                    model.password
                        |> validate (OnChange p) passwordValidation
            }
                => Cmd.none
                => NoOp

        BlurPassword ->
            { model
                | password =
                    model.password
                        |> validate OnBlur passwordValidation
            }
                => Cmd.none
                => NoOp

        ChangePasswordCompleted (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            response.body
                                |> decodeString
                                    (Decode.field "errors" errorsDecoder)
                                |> Result.withDefault []

                        _ ->
                            [ ( "Unknown Error", "Unable to process login" ) ]
            in
            model
                |> applyServerValidationErrors errorMessages
                => Cmd.none
                => NoOp

        ChangePasswordCompleted (Ok _) ->
            model
                => Route.modifyUrl Route.Home
                => NoOp


submitIfValid : Session -> (Msg m -> m) -> Model m -> ( Model m, Cmd m )
submitIfValid session lift model =
    let
        submissionResult =
            Valid (submit session lift)
                |: validity model.password
    in
    case submissionResult of
        Valid cmd ->
            ( { model | isLoading = True }, cmd )

        _ ->
            ( model, Cmd.none )


submit : Session -> (Msg m -> m) -> String -> Cmd m
submit session lift password =
    let
        newModel =
            { password = password }

        token =
            session.user |> Maybe.map .token
    in
    Http.send (ChangePasswordCompleted >> lift)
        (Requests.User.changePassword newModel token)


validateModel : Model m -> Model m
validateModel model =
    let
        password =
            model.password |> validate OnSubmit passwordValidation
    in
    { model
        | password = password
    }


applyServerValidationErrors : List ( String, String ) -> Model m -> Model m
applyServerValidationErrors errs model =
    let
        applyError err model =
            let
                ( formField, message ) =
                    err
            in
            case formField of
                "password" ->
                    { model
                        | password =
                            model.password
                                |> setError
                                    (String.join " "
                                        [ "Password"
                                        , message
                                        ]
                                    )
                    }

                "base" ->
                    { model
                        | base =
                            model.base
                                |> setError message
                    }

                _ ->
                    model
    in
    errs |> List.foldl applyError model


passwordValidation : Pages.Shared.Validation.Validator String String
passwordValidation =
    isNotEmpty "A password is required"
        >=> isAtLeast8Chars "Password must be at least 8 characters"


errorsDecoder : Decoder (List ( String, String ))
errorsDecoder =
    decode (\email base -> List.concat [ email, base ])
        |> optionalError "password"
        |> optionalError "base"


optionalError : String -> Decoder (List ( String, String ) -> a) -> Decoder a
optionalError fieldName =
    let
        errorToTuple errorMessage =
            ( fieldName, errorMessage )
    in
    Pipeline.optional fieldName
        (Decode.list
            (Decode.map errorToTuple string)
        )
        []


helperText : Field raw a -> String -> Html m
helperText rawField defaultText =
    TextfieldHelper.helperText
        [ TextfieldHelper.persistent
        , TextfieldHelper.validationMsg
        ]
        [ rawField
            |> extractError
            |> Maybe.withDefault defaultText
            |> text
        ]


errorClass : Field raw a -> List (Textfield.Property m)
errorClass rawField =
    case validity rawField of
        Invalid _ ->
            [ Textfield.invalid ]

        _ ->
            []
