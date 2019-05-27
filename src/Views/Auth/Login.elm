module Views.Auth.Login exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , initialModel
    , topbar
    , update
    , view
    )

{-| The login page.
-}

import Data.User exposing (User)
import Html exposing (Html, a, div, form, p, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onSubmit)
import Html.Keyed as Keyed
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, string)
import Json.Decode.Pipeline as Pipeline exposing (decode)
import Main.Route as Route
import Main.Util exposing ((=>))
import Material
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



-- MODEL --


type alias Model m =
    { mdc : Material.Model m
    , email : Field String String
    , password : Field String String
    , base : Field String String
    , isLoading : Bool
    }


initialModel : Model m
initialModel =
    { mdc = Material.defaultModel
    , email = field ""
    , password = field ""
    , base = field ""
    , isLoading = False
    }


type Msg m
    = Mdc (Material.Msg m)
    | SetEmail String
    | BlurEmail
    | SetPassword String
    | BlurPassword
    | SubmitForm
    | LoginCompleted (Result Http.Error User)


type ExternalMsg
    = NoOp
    | SetUser User


update : (Msg m -> m) -> Msg m -> Model m -> ( ( Model m, Cmd m ), ExternalMsg )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (Mdc >> lift) msg_ model => NoOp

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

        SetPassword newPass ->
            { model
                | password =
                    model.password
                        |> validate (OnChange newPass) passwordValidation
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

        LoginCompleted (Err error) ->
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
            { model | isLoading = False }
                |> applyServerValidationErrors errorMessages
                => Cmd.none
                => NoOp

        LoginCompleted (Ok user) ->
            model
                => Cmd.batch [ storeSession user, Route.modifyUrl Route.Home ]
                => SetUser user


topbar : (Msg m -> m) -> Model m -> Bool -> Html m
topbar lift model isLoading =
    Toolbar.render (Mdc >> lift)
        "au-Toolbar"
        model.mdc
        "Login"
        isLoading


view : (Msg m -> m) -> Model m -> Bool -> Html m
view lift model forgotEmailSent =
    div [ class "cell auto g-ContentCol" ]
        [ div [ class "grid-x" ]
            [ div [ class "cell" ]
                [ forgotEmailSentMessage forgotEmailSent
                , baseError model.base ""
                ]
            ]
        , form [ onSubmit (lift SubmitForm) ]
            [ Keyed.node "div"
                [ class "grid-x au-Input" ]
                [ ( "login-email"
                  , div [ class "cell" ]
                        [ Textfield.view (Mdc >> lift)
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
            , div [ class "grid-x au-Input" ]
                [ div [ class "cell" ]
                    [ Textfield.view (Mdc >> lift)
                        "au-PasswordText"
                        model.mdc
                        ([ Textfield.label "Password *"
                         , Options.onInput (SetPassword >> lift)
                         , Options.onBlur (lift BlurPassword)
                         , Textfield.password
                         ]
                            ++ errorClass model.password
                        )
                        []
                    , helperText model.password ""
                    ]
                ]
            , div [ class "grid-x au-SubmitButton" ]
                [ div [ class "cell" ]
                    [ Button.view (Mdc >> lift)
                        "au-SubmitButton"
                        model.mdc
                        [ Button.ripple
                        , Options.onClick (lift SubmitForm)
                        , Button.raised
                        ]
                        [ text "Login"
                        ]
                    ]
                ]
            , div [ class "grid-x au-HelpLinks" ]
                [ div [ class "cell au-HelpLinks_Link" ]
                    [ a
                        [ Route.href Route.ForgotPassword
                        ]
                        [ text "Forgot your password?" ]
                    ]
                , div [ class "cell au-HelpLinks_Link" ]
                    [ a
                        [ Route.href Route.Signup
                        ]
                        [ text "Need an account?" ]
                    ]
                ]
            ]
        ]


submitIfValid : (Msg m -> m) -> Model m -> ( Model m, Cmd m )
submitIfValid lift model =
    let
        submissionResult =
            Valid (submit lift)
                |: validity model.email
                |: validity model.password
    in
    case submissionResult of
        Valid cmd ->
            ( { model | isLoading = True }, cmd )

        _ ->
            ( model, Cmd.none )


submit : (Msg m -> m) -> String -> String -> Cmd m
submit lift email password =
    let
        newModel =
            { email = email, password = password }
    in
    Http.send (LoginCompleted >> lift) (Requests.User.login newModel)


validateModel : Model m -> Model m
validateModel model =
    let
        email =
            model.email |> validate OnSubmit emailValidation

        password =
            model.password |> validate OnSubmit passwordValidation
    in
    { model
        | email = email
        , password = password
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


emailValidation : Pages.Shared.Validation.Validator String String
emailValidation =
    isNotEmpty "An email is required"


passwordValidation : Pages.Shared.Validation.Validator String String
passwordValidation =
    isNotEmpty "A password is required"


errorsDecoder : Decoder (List ( String, String ))
errorsDecoder =
    decode (\email password base -> List.concat [ email, password, base ])
        |> optionalError "email"
        |> optionalError "password"
        |> optionalError "base"


optionalError : String -> Decoder (List ( String, String ) -> a) -> Decoder a
optionalError fieldName =
    let
        errorToTuple errorMessage =
            ( fieldName, errorMessage )
    in
    Pipeline.optional fieldName
        (Decode.list (Decode.map errorToTuple string))
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


forgotEmailSentMessage : Bool -> Html m
forgotEmailSentMessage emailSent =
    if emailSent then
        p [ class "au-ForgotEmailSent" ] [ text "An email was sent with a link to change your password." ]

    else
        span [] []


baseError : Field raw a -> String -> Html m
baseError rawField defaultText =
    p [ class "au-BaseError" ]
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
