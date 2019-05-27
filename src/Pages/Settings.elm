module Pages.Settings exposing (initialModel)

import Data.Setting exposing (Setting)
import Data.User exposing (User, usernameToString)
import Views.Model as ViewModel exposing (Model)


initialModel : List Setting -> Maybe User -> Model m
initialModel settings maybeUser =
    let
        model =
            ViewModel.defaultModel

        username =
            case maybeUser of
                Just user ->
                    usernameToString user.username

                Nothing ->
                    ""
    in
    { model | settings = settings, username = username }
