module Views.Auth.ForgotPassword exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , initialModel
    , topbar
    , update
    , view
    )

{-| The forgot password request page.
-}

import Data.User exposing (User)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
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
import Pages.Shared.Toolbars.Auth as Toolbar
import Pages.Shared.Validation
    exposing
        ( (|:)
        , Event(..)
        , Field
        , Validity(..)
        , extractError
        , field
        , isNotEmpty
        , setError
        , validate
        , validity
        )
import Requests.User exposing (storeSession)


type alias Model m =
    { mdc : Material.Model m
    , email : Field String String
    , base : Field String String
    , isLoading : Bool
    }


initialModel : Model m
initialModel =
    { mdc = Material.defaultModel
    , email = field ""
    , base = field ""
    , isLoading = False
    }


type Msg m
    = Mdl (Material.Msg m)
    | SetEmail String
    | BlurEmail
    | SubmitForm
    | ForgotPasswordCompleted (Result Http.Error User)


type ExternalMsg
    = NoOp
    | ForgotEmailSent


update : (Msg m -> m) -> Msg m -> Model m -> ( ( Model m, Cmd m ), ExternalMsg )
update lift msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model => NoOp

        SubmitForm ->
            model |> validateModel |> submitIfValid lift => NoOp

        SetEmail e ->
            { model
                | email =
                    model.email
                        |> validate (OnChange e) emailValidation
            }
                => Cmd.none
                => NoOp

        BlurEmail ->
            { model
                | email =
                    model.email
                        |> validate OnBlur emailValidation
            }
                => Cmd.none
                => NoOp

        ForgotPasswordCompleted (Err error) ->
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

        ForgotPasswordCompleted (Ok user) ->
            { model | isLoading = False }
                => Route.modifyUrl Route.Login
                => ForgotEmailSent


topbar : (Msg m -> m) -> Model m -> Bool -> Html m
topbar lift model isLoading =
    Toolbar.render (Mdl >> lift)
        "fp-Toolbar"
        model.mdc
        "Forgot Password"
        isLoading


view : (Msg m -> m) -> Model m -> Html m
view lift model =
    div [ class "cell auto g-ContentCol" ]
        [ div [ class "grid-x au-Instructions" ] [ text "Enter your email address and if your account is found, an email with a password reset link will be sent." ]
        , Keyed.node "div"
            [ class "grid-x au-Input" ]
            [ ( "forgotpassword-email"
              , div [ class "cell" ]
                    [ Textfield.view (Mdl >> lift)
                        "au-EmailText"
                        model.mdc
                        ([ Textfield.label "Email Address *"
                         , Options.onInput (SetEmail >> lift)
                         , Options.onBlur (lift BlurEmail)
                         ]
                            ++ errorClass model.email
                        )
                        []
                    , helperText model.email ""
                    ]
              )
            ]
        , div [ class "grid-x au-SubmitButton" ]
            [ div [ class "cell" ]
                [ Button.view (Mdl >> lift)
                    "fp-SubmitButton"
                    model.mdc
                    [ Button.ripple
                    , Options.onClick (lift SubmitForm)
                    , Button.raised
                    ]
                    [ text "Send Forgot Password email"
                    ]
                ]
            ]
        , div [ class "grid-x au-HelpLinks" ]
            [ div [ class "au-HelpLinks_Link cell" ]
                [ a
                    [ Route.href Route.Login
                    ]
                    [ text "Want to login?" ]
                ]
            ]
        ]



-- VALIDATION --


submitIfValid : (Msg m -> m) -> Model m -> ( Model m, Cmd m )
submitIfValid lift model =
    let
        submissionResult =
            Valid (submit lift)
                |: validity model.email
    in
    case submissionResult of
        Valid cmd ->
            ( { model | isLoading = True }, cmd )

        _ ->
            ( model, Cmd.none )


submit : (Msg m -> m) -> String -> Cmd m
submit lift email =
    let
        newModel =
            { email = email }
    in
    Http.send (ForgotPasswordCompleted >> lift)
        (Requests.User.forgotPassword newModel)


validateModel : Model m -> Model m
validateModel model =
    let
        email =
            model.email |> validate OnSubmit emailValidation
    in
    { model
        | email = email
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
                "email" ->
                    { model
                        | email =
                            model.email
                                |> setError
                                    (String.join " " [ "Email", message ])
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


emailValidation : Pages.Shared.Validation.Validator String String
emailValidation =
    isNotEmpty "An email is required"


errorsDecoder : Decoder (List ( String, String ))
errorsDecoder =
    decode (\email base -> List.concat [ email, base ])
        |> optionalError "email"
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
