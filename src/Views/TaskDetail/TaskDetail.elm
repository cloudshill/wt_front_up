module Views.TaskDetail.TaskDetail exposing
    ( ExternalMsg(..)
    , Msg
    , actionButton
    , empty
    , mediumTopbar
    , smallTopbar
    , update
    , view
    )

import Data.Session as Session exposing (Session)
import Data.TagsProjects exposing (TagsProjects)
import Data.Task exposing (Task)
import Dict
import Html
    exposing
        ( Html
        , div
        , i
        , text
        )
import Html.Attributes exposing (class, classList)
import Http
import Main.Route as Route exposing (Route)
import Main.Util as Util exposing ((=>), pair)
import Material
import Material.Icon as Icon
import Material.Options as Options
import Material.Toolbar as Toolbar
import Requests.Task
import Views.ContentColTopbar.ContentColTopbar as MediumTopbar
import Views.Helpers exposing (ThreeColConfig)
import Views.Model exposing (Model)
import Views.Task.Task as ViewTask
import Views.TaskDetail.Topbar as SmallTopbar


type Msg m
    = Mdl (Material.Msg m)
    | GoTo Route
    | GoBack
    | MarkDone
    | EditTask
    | CancelEditTask
    | Delete
    | TaskMarkedDone (Result Http.Error TagsProjects)
    | TaskDeleted (Result Http.Error TagsProjects)


type ExternalMsg
    = NoOp
    | RemoveTask String String TagsProjects


update : Session -> (Msg m -> m) -> Msg m -> Model m -> ( ( Model m, Cmd m ), ExternalMsg )
update session lift msg model =
    let
        uuid =
            case model.task of
                Nothing ->
                    ""

                Just task ->
                    task.uuid
    in
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model => NoOp

        GoTo route ->
            ( model, Route.newUrl route ) => NoOp

        GoBack ->
            model => Route.backOne => NoOp

        CancelEditTask ->
            { model | editTask = False } => Cmd.none => NoOp

        EditTask ->
            { model | editTask = True } => Cmd.none => NoOp

        MarkDone ->
            let
                cmdFromAuth authToken =
                    Requests.Task.markDone authToken uuid
                        |> Http.send (lift << TaskMarkedDone)
            in
            session
                |> Session.attempt "marktaskdone" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors model)
                => NoOp

        Delete ->
            let
                cmdFromAuth authToken =
                    Requests.Task.delete authToken uuid
                        |> Http.send (lift << TaskDeleted)
            in
            session
                |> Session.attempt "deletetask" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors model)
                => NoOp

        TaskDeleted (Ok tagsProjects) ->
            Route.Home
                |> Route.newUrl
                |> pair model
                => RemoveTask uuid "Task deleted." tagsProjects

        TaskDeleted (Err _) ->
            ( { model
                | errors =
                    model.errors
                        ++ [ "Server error while trying to set context." ]
              }
            , Cmd.none
            )
                => NoOp

        TaskMarkedDone (Ok tagsProjects) ->
            Route.Home
                |> Route.newUrl
                |> pair model
                => RemoveTask uuid "Task marked done." tagsProjects

        TaskMarkedDone (Err _) ->
            ( { model
                | errors =
                    model.errors
                        ++ [ "Server error while trying to set context." ]
              }
            , Cmd.none
            )
                => NoOp


smallTopbar : (Msg m -> m) -> ThreeColConfig -> Material.Model m -> Html m
smallTopbar lift config mdc =
    let
        task =
            case config.task of
                Nothing ->
                    Task "" "" Nothing [] Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Dict.empty

                Just justTask ->
                    justTask
    in
    SmallTopbar.render
        (Mdl >> lift)
        "td-Appbar"
        mdc
        (lift GoBack)
        task.description
        (lift MarkDone)
        (lift Delete)


mediumTopbar : (Msg m -> m) -> ThreeColConfig -> Model m -> Html m
mediumTopbar lift config model =
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
            [ actionButton (lift (GoTo config.taskFormRoute))
            ]
    in
    MediumTopbar.render
        (Mdl >> lift)
        "td-MediumTopbar"
        model.mdc
        startSection
        endSection


actionButton : m -> Html m
actionButton goToForm =
    Icon.view [ Toolbar.icon, Options.onClick goToForm ] "edit"


empty : Html m
empty =
    div [ class "show-for-medium cell medium-1 xlarge-3" ] []


view : (Msg m -> m) -> ThreeColConfig -> Model m -> Html m
view lift config model =
    div [ classList [ ( "g-ContentCol cell medium-9 xlarge-3 grid-y", True ), ( "show-for-xlarge", config.mobileConfig.hideThird ) ] ]
        [ div [ class "medium-12 cell cell-block-y" ]
            [ mediumTopbar lift config model
            , content config
            ]
        ]


content : ThreeColConfig -> Html m
content config =
    case config.task of
        Nothing ->
            div [ class "align-middle-center td-ClickMessage mdc-typography--headline6" ] [ text "Click task to see detailed view" ]

        Just task ->
            ViewTask.view task
