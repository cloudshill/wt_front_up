module Views.Settings.Settings exposing (downloadConfigButton, view)

import Config exposing (config)
import Data.Setting exposing (Setting, defaultSetting)
import Html exposing (Html, a, div, h5, hr, text)
import Html.Attributes exposing (class, href, target)
import Main.Route as Route
import Views.Model as ViewModel


view : ViewModel.Model m -> Html m
view model =
    let
        email =
            (settingByKey model.settings "email").value

        username =
            model.username

        configLink =
            downloadConfigButton model.settings

        changePasswordLink =
            a [ Route.href Route.ChangePasswordWithoutToken, class "mdc-button" ] [ text "Change Password" ]
    in
    div [ class "g-ContentCol cell medium-9 grid-y " ]
        [ div [ class "medium-1 cell grid-x grid-padding-x show-for-medium" ]
            [ div [ class "cell medium-12" ]
                []
            ]
        , div [ class "se-Settings medium-11 cell cell-block-y" ]
            [ div [ class "se-Help" ] [ settingBlock "Version" (text config.version) ]
            , div [ class "se-Help" ]
                [ a
                    [ href "https://docs.wingtask.com"
                    , class "mdc-button"
                    , target "_blank"
                    ]
                    [ text "Go to Docs" ]
                ]
            , div [ class "se-Help" ]
                [ a
                    [ href "https://forum.wingtask.com"
                    , class "mdc-button"
                    , target "_blank"
                    ]
                    [ text "Report a bug / Request a feature" ]
                ]
            , hr [] []
            , settingBlock "Email" (text email)
            , settingBlock "Username" (text username)
            , settingBlock "Password" changePasswordLink
            , settingBlock "Taskwarrior Configuration" configLink
            ]
        ]


downloadConfigButton : List Setting -> Html m
downloadConfigButton settings =
    a
        [ href (settingByKey settings "config_file_path").value
        , class "mdc-js-button mdc-button mdc-button--raised mdc-theme--secondary-bg bs-Greet_Button"
        ]
        [ text "Download WingTask Configuration" ]


settingBlock : String -> Html m -> Html m
settingBlock key value =
    div []
        [ h5 [] [ text key ]
        , value
        ]


settingByKey : List Setting -> String -> Setting
settingByKey settings key =
    settings
        |> List.filter (\s -> s.key == key)
        |> List.head
        |> Maybe.withDefault defaultSetting
