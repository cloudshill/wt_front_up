module Views.Welcome.Welcome exposing
    ( Msg
    , initialModel
    , update
    , view
    )

import Data.Setting exposing (Setting, defaultSetting)
import Html exposing (Html, a, div, h5, p, text)
import Html.Attributes exposing (class, href)
import Main.Route as Route exposing (Route)
import Material
import Material.Button as Button
import Material.Options exposing (cs, onClick)
import Material.Theme as Theme
import Views.Model as ViewModel exposing (Model)
import Views.Settings.Settings exposing (downloadConfigButton)


type Msg m
    = Mdc (Material.Msg m)
    | GoTo Route


initialModel : List Setting -> Model m
initialModel settings =
    let
        model =
            ViewModel.defaultModel
    in
    { model | settings = settings }


update :
    (Msg m -> m)
    -> Msg m
    -> Model m
    -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdc msg_ ->
            Material.update (Mdc >> lift) msg_ model

        GoTo route ->
            ( model, Route.newUrl route )


view : (Msg m -> m) -> Model m -> Html m
view lift model =
    div [ class "g-ContentCol cell auto" ]
        [ div [ class "bs-Greet" ]
            [ div [ class "bs-Greet_Headline mdc-typography--headline4" ] [ text "Getting Started with WingTask" ]
            , div [ class "mdc-typography--headline5 bs-Greet_Question" ] [ text "Are you a current Taskwarrior user?" ]
            , div [ class "mdc-typography--headline5 bs-Greet_Answer" ] [ text "YES" ]
            , p [ class "mdc-typography--body1 bs-Greet_Explainer" ] [ text "You can get your tasks into WingTask by configuring Taskwarrior to sync with WingTask servers.  The WingTask Configuration download contains the necessary keys, certs, and credentials for syncing your tasks up with WingTask servers, it also includes an install script to make configuration quick and easy." ]
            , downloadConfigButton model.settings
            , div [ class "mdc-typography--headline4 bs-Greet_Answer" ] [ text "NO" ]
            , p [ class "mdc-typography--body1 bs-Greet_Explainer" ] [ text "You can start using WingTask right away by creating a new task. Later if you wish to use Taskwarrior from the command line (recommended), you can download your WingTask credentials from Settings" ]
            , a
                [ Route.href Route.NewTask
                , class "mdc-js-button mdc-button mdc-button--raised mdc-theme--secondary-bg bs-Greet_Button"
                ]
                [ text "Create New Task" ]
            ]
        ]
