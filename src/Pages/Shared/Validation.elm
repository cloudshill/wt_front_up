-- Validation.elm


module Pages.Shared.Validation exposing
    ( (>=>)
    , (|:)
    , Event(..)
    , Field
    , Validator
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

import Regex


type Field raw a
    = Field raw (Validity a)


field : a -> Field a a1
field value =
    Field value NotValidated


validity : Field raw a -> Validity a
validity (Field _ validity) =
    validity


setError : String -> Field b a -> Field b a1
setError err (Field value _) =
    Field value (Invalid err)


type Validity a
    = NotValidated
    | Valid a
    | Invalid String


extractError : Field raw a -> Maybe String
extractError field =
    case validity field of
        Invalid err ->
            Just err

        _ ->
            Nothing



-- Validator


type alias Validator a b =
    a -> Result String b


(>=>) : Validator a b -> Validator b c -> Validator a c
(>=>) f g =
    f >> Result.andThen g



-- Validation logic


type Event raw
    = OnSubmit
    | OnBlur
    | OnRelatedChange
    | OnChange raw


validate : Event raw -> Validator raw a -> Field raw a -> Field raw a
validate event validate (Field value validity) =
    case event of
        OnSubmit ->
            validateAlways validate (Field value validity)

        OnBlur ->
            validateAlways validate (Field value validity)

        OnChange newValue ->
            validateIfValidated validate (Field newValue validity)

        OnRelatedChange ->
            validateIfValidated validate (Field value validity)


validateIfValidated : Validator raw a -> Field raw a -> Field raw a
validateIfValidated validate (Field value validity) =
    Field value
        (case validity of
            NotValidated ->
                NotValidated

            _ ->
                validate value |> toValidity
        )


validateAlways : Validator raw a -> Field raw a -> Field raw a
validateAlways validate (Field value _) =
    Field value (validate value |> toValidity)


toValidity : Result String a -> Validity a
toValidity result =
    case result of
        Ok a ->
            Valid a

        Err err ->
            Invalid err


apply : Validity a -> Validity (a -> b) -> Validity b
apply fa ff =
    case fa of
        NotValidated ->
            NotValidated

        Invalid err ->
            Invalid err

        Valid a ->
            case ff of
                NotValidated ->
                    NotValidated

                Invalid err ->
                    Invalid err

                Valid f ->
                    f a |> Valid


(|:) : Validity (a -> b) -> Validity a -> Validity b
(|:) =
    \b a -> apply a b



-- Common validators


type alias ErrorMessage =
    String


isNotEmpty : ErrorMessage -> Validator String String
isNotEmpty err value =
    if value == "" then
        Err err

    else
        Ok value


isEmail : ErrorMessage -> Validator String String
isEmail err value =
    let
        regex =
            Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
                |> Regex.caseInsensitive
    in
    if Regex.contains regex value then
        Ok value

    else
        Err err


isAtLeast4Chars : ErrorMessage -> Validator String String
isAtLeast4Chars err value =
    if String.length value < 4 then
        Err err

    else
        Ok value


isAtLeast8Chars : ErrorMessage -> Validator String String
isAtLeast8Chars err value =
    if String.length value < 8 then
        Err err

    else
        Ok value
