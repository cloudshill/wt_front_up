module Views.Auth.Signup exposing
    ( ExternalMsg(..)
    , Model
    , Msg(..)
    , initialModel
    , topbar
    , update
    , view
    )

import Data.User exposing (User)
import Html exposing (Html, a, div, text)
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
        , isAtLeast4Chars
        , isAtLeast8Chars
        , isEmail
        , isNotEmpty
        , setError
        , validate
        , validity
        )
import Requests.User exposing (storeSession)


type alias Model m =
    { mdc : Material.Model m
    , email : Field String String
    , username : Field String String
    , password : Field String String
    , base : Field String String
    , isLoading : Bool
    }


initialModel : Model m
initialModel =
    { mdc = Material.defaultModel
    , email = field ""
    , username = field ""
    , password = field ""
    , base = field ""
    , isLoading = False
    }


type Msg m
    = Mdl (Material.Msg m)
    | SetEmail String
    | BlurEmail
    | SetUsername String
    | BlurUsername
    | SetPassword String
    | BlurPassword
    | SubmitForm
    | SignupCompleted (Result Http.Error User)


type ExternalMsg
    = NoOp
    | SetUser User


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

        SetUsername u ->
            { model
                | username =
                    model.username
                        |> validate (OnChange u) usernameValidation
            }
                => Cmd.none
                => NoOp

        BlurUsername ->
            { model
                | username =
                    model.username
                        |> validate OnBlur usernameValidation
            }
                => Cmd.none
                => NoOp

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

        SignupCompleted (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            response.body
                                |> decodeString
                                    (Decode.field "errors" errorsDecoder)
                                |> Result.withDefault []

                        _ ->
                            [ ( "Unknown Error", "Unable to process signup" ) ]
            in
            { model | isLoading = False }
                |> applyServerValidationErrors errorMessages
                => Cmd.none
                => NoOp

        SignupCompleted (Ok user) ->
            model
                => Cmd.batch [ storeSession user, Route.modifyUrl Route.Home ]
                => SetUser user


submitIfValid : (Msg m -> m) -> Model m -> ( Model m, Cmd m )
submitIfValid lift model =
    let
        submissionResult =
            Valid (submit lift)
                |: validity model.email
                |: validity model.password
                |: validity model.username
    in
    case submissionResult of
        Valid cmd ->
            ( { model | isLoading = True }, cmd )

        _ ->
            ( model, Cmd.none )


submit : (Msg m -> m) -> String -> String -> String -> Cmd m
submit lift email password username =
    let
        newModel =
            { email = email, password = password, username = username }
    in
    Http.send (SignupCompleted >> lift) (Requests.User.signup newModel)


validateModel : Model m -> Model m
validateModel model =
    let
        email =
            model.email |> validate OnSubmit emailValidation

        password =
            model.password |> validate OnSubmit passwordValidation

        username =
            model.username |> validate OnSubmit usernameValidation
    in
    { model
        | email = email
        , password = password
        , username = username
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

                "username" ->
                    { model
                        | username =
                            model.username
                                |> setError
                                    (String.join " "
                                        [ "Username"
                                        , message
                                        ]
                                    )
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


errorsDecoder : Decoder (List ( String, String ))
errorsDecoder =
    decode
        (\email username password base ->
            List.concat [ email, username, password, base ]
        )
        |> optionalError "email"
        |> optionalError "username"
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


topbar : (Msg m -> m) -> Model m -> Bool -> Html m
topbar lift model isLoading =
    Toolbar.render (Mdl >> lift) "su-Toobar" model.mdc "Signup" isLoading


view : (Msg m -> m) -> Model m -> Html m
view lift model =
    div [ class "auto cell g-ContentCol" ]
        [ div [ class "grid-x au-Input" ]
            [ div [ class "cell" ]
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
            ]
        , div [ class "grid-x au-Input" ]
            [ div [ class "cell" ]
                [ Textfield.view (Mdl >> lift)
                    "su-Username"
                    model.mdc
                    ([ Textfield.label "Username *"
                     , Options.onInput (SetUsername >> lift)
                     , Options.onBlur (lift BlurUsername)
                     ]
                        ++ errorClass model.username
                    )
                    []
                , helperText model.username ""
                ]
            ]
        , div [ class "grid-x au-Input" ]
            [ div [ class "cell" ]
                [ Textfield.view (Mdl >> lift)
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
                [ Button.view (Mdl >> lift)
                    "su-SubmitButton"
                    model.mdc
                    [ Button.ripple
                    , Options.onClick (lift SubmitForm)
                    , Button.raised
                    ]
                    [ text "Signup"
                    ]
                ]
            ]
        , div [ class "grid-x au-HelpLinks" ]
            [ div [ class "cell au-HelpLinks_Link" ]
                [ text "Already have an account?"
                , a
                    [ Route.href Route.Login
                    ]
                    [ text " Login" ]
                ]
            ]
        ]


emailValidation : Pages.Shared.Validation.Validator String String
emailValidation =
    isNotEmpty "An email is required"
        >=> isEmail "Email is not a valid format"


usernameValidation : Pages.Shared.Validation.Validator String String
usernameValidation =
    isNotEmpty "A username is required"
        >=> isAtLeast4Chars "Username must be at least 4 characters"


passwordValidation : Pages.Shared.Validation.Validator String String
passwordValidation =
    isNotEmpty "A password is required"
        >=> isAtLeast8Chars "Password must be at least 8 characters"


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
