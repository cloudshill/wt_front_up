module Views.ContextDetail.ContextDetail exposing
    ( ExternalMsg(..)
    , Msg
    , smallTopbar
    , subscriptions
    , update
    , view
    )

import Data.Context as Context exposing (Context)
import Data.Session as Session exposing (Session)
import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Http
import Main.Route as Route exposing (Route)
import Main.Util as Util exposing ((=>), pair)
import Material
import Material.Icon as Icon
import Material.Options as Options
import Material.Toolbar as Toolbar
import Pages.Shared.Page exposing (FixedAdjust(..))
import Requests.Context
import Views.ContentColTopbar.ContentColTopbar as MediumTopbar
import Views.ContextDetail.SmallTopbar as SmallTopbar
import Views.Helpers exposing (ThreeColConfig)
import Views.Model exposing (Model)
import Views.Task.Task exposing (field)
import Views.TaskDetail.TaskDetail exposing (mediumTopbar)


type Msg m
    = Mdl (Material.Msg m)
    | GoTo Route
    | GoBack
    | Delete
    | ContextDeleted (Result Http.Error ())


type ExternalMsg
    = NoOp
    | RemoveContext String String


update : Session -> (Msg m -> m) -> Msg m -> Model m -> ( ( Model m, Cmd m ), ExternalMsg )
update session lift msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model => NoOp

        GoTo route ->
            ( model, Route.modifyUrl route ) => NoOp

        GoBack ->
            ( model, Route.backOne ) => NoOp

        Delete ->
            let
                cmdFromAuth authToken =
                    Requests.Context.delete authToken model.slugOrQuery
                        |> Http.send (lift << ContextDeleted)
            in
            session
                |> Session.attempt "deletecontext" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors model)
                => NoOp

        ContextDeleted (Ok ()) ->
            Route.Contexts
                |> Route.newUrl
                |> pair model
                => RemoveContext model.slugOrQuery "Context deleted."

        ContextDeleted (Err _) ->
            ( { model
                | errors =
                    model.errors
                        ++ [ "Server error while trying to delete context." ]
              }
            , Cmd.none
            )
                => NoOp


view : (Msg m -> m) -> ThreeColConfig -> Model m -> Html m
view lift config model =
    let
        context =
            case config.context of
                Nothing ->
                    Context.defaultContext

                Just justContext ->
                    justContext
    in
    div
        [ classList
            [ ( "g-ContentCol cell xlarge-3 medium-9 grid-y", True )
            , ( "show-for-xlarge", config.mobileConfig.hideThird )
            ]
        ]
        [ div [ class "medium-1 cell align-middle border-bottom grid-x grid-padding-x show-for-medium" ]
            [ div [ class "cell" ]
                [ i [ onClick (lift GoBack), class "td-GoBack hide-for-xlarge material-icons md-36" ] [ text "arrow_back" ] ]
            ]
        , div [ class "medium-11 cell task-detail cell-block-y" ]
            [ mediumTopbar lift context model
            , content context
            ]
        ]


actionButton : m -> Html m
actionButton goToForm =
    Icon.view [ Toolbar.icon, Options.onClick goToForm ] "edit"


mediumTopbar : (Msg m -> m) -> Context -> Model m -> Html m
mediumTopbar lift context model =
    let
        startSection =
            [ Icon.view
                [ Options.onClick (lift GoBack)
                , Options.cs "hide-for-xlarge"
                , Toolbar.icon
                ]
                "arrow_back"
            ]

        endSection =
            [ actionButton (lift (GoTo (Route.EditContext context.slug)))
            ]
    in
    MediumTopbar.render
        (Mdl >> lift)
        "td-MediumTopbar"
        model.mdc
        startSection
        endSection


smallTopbar : (Msg m -> m) -> ThreeColConfig -> Material.Model m -> Html m
smallTopbar lift config mdc =
    let
        context =
            case config.context of
                Nothing ->
                    Context.defaultContext

                Just justContext ->
                    justContext
    in
    SmallTopbar.render
        (Mdl >> lift)
        "cd-SmallTopbar"
        mdc
        (lift GoBack)
        context.name
        (lift Delete)


content : Context -> Html m
content context =
    div [ class "tk-Content grid-container" ]
        [ field "Name" context.name
        , field "Definition" context.definition
        ]


subscriptions : (Msg m -> m) -> Model m -> Sub m
subscriptions lift model =
    Sub.batch
        [ Material.subscriptions (Mdl >> lift) model
        ]
