module Views.Tasks.Tasks exposing
    ( ExternalMsg(..)
    , Msg
    , empty
    , isTaskListEmpty
    , isTaskListNotEmpty
    , update
    , view
    )

import Data.Session as Session exposing (Session)
import Data.TagsProjects exposing (TagsProjects)
import Data.Task exposing (Task)
import Html exposing (Html, div, h3, hr, i, img, p, span, text)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import Http
import Main.Route as Route exposing (Route)
import Main.Util as Util exposing ((=>))
import Material exposing (Msg)
import Material.Button as Button
import Material.Icon as Icon
import Material.Options as Options exposing (cs)
import Material.Toolbar as Toolbar
import Pages.Shared.Page exposing (blankState)
import Requests.Task
import Views.ContentColTopbar.ContentColTopbar as MediumTopbar
import Views.Helpers exposing (ThreeColConfig)
import Views.Model exposing (Model)


type Msg m
    = Mdl (Material.Msg m)
    | GoTo Route
    | GoBack
    | MarkDone String
    | Delete String
    | TaskMarkedDone String (Result Http.Error TagsProjects)
    | TaskDeleted String (Result Http.Error TagsProjects)


type ExternalMsg
    = NoOp
    | RemoveTask String String
    | UpdateTagsProjects TagsProjects


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

        GoTo route ->
            ( model, Route.newUrl route ) => NoOp

        GoBack ->
            model => Route.backOne => NoOp

        MarkDone uuid ->
            let
                newModel =
                    { model
                        | tasks =
                            List.filter (\t -> t.uuid /= uuid)
                                model.tasks
                        , requestedTasks =
                            List.filter (\t -> t.uuid /= uuid)
                                model.requestedTasks
                    }

                cmdFromAuth authToken =
                    Requests.Task.markDone authToken uuid
                        |> Http.send (lift << TaskMarkedDone uuid)
            in
            session
                |> Session.attempt "marktaskdone" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors newModel)
                => RemoveTask uuid "Task marked done."

        Delete uuid ->
            let
                newModel =
                    { model
                        | tasks =
                            List.filter (\t -> t.uuid /= uuid)
                                model.tasks
                        , requestedTasks =
                            List.filter (\t -> t.uuid /= uuid)
                                model.requestedTasks
                    }

                cmdFromAuth authToken =
                    Requests.Task.delete authToken uuid
                        |> Http.send (lift << TaskDeleted uuid)
            in
            session
                |> Session.attempt "deletetask" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors newModel)
                => RemoveTask uuid "Task deleted."

        TaskDeleted uuid (Ok tagsProjects) ->
            ( model
            , Cmd.none
            )
                => UpdateTagsProjects tagsProjects

        TaskDeleted _ (Err _) ->
            ( { model
                | errors =
                    model.errors
                        ++ [ "Server error while trying to set context." ]
              }
            , Cmd.none
            )
                => NoOp

        TaskMarkedDone uuid (Ok tagsProjects) ->
            ( model
            , Cmd.none
            )
                => UpdateTagsProjects tagsProjects

        TaskMarkedDone uuid (Err _) ->
            ( { model
                | errors =
                    model.errors
                        ++ [ "Server error while trying to set context." ]
              }
            , Cmd.none
            )
                => NoOp


empty : Html m
empty =
    div [ class "cell auto" ] []


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
            , Toolbar.title [] [ text config.topbarTitle ]
            ]

        endSection =
            []
    in
    MediumTopbar.render
        (Mdl >> lift)
        "td-MediumTopbar"
        model.mdc
        startSection
        endSection


view : (Msg m -> m) -> Model m -> ThreeColConfig -> (Route.Route -> m) -> (String -> Route.Route) -> Html m
view lift model config goTo taskRoute =
    div
        [ classList
            [ ( "cell ta-TaskList auto", True )
            , ( "show-for-xlarge", config.mobileConfig.hideSecond )
            , ( "g-ContentCol", isTaskListNotEmpty config.tasks )
            ]
        ]
        [ div [ class "ta-Tasklist-xlarge-1 cell" ]
            [ mediumTopbar lift config model
            ]
        , div [ class "ta-TaskList_xlarge-11 cell cell-block-y" ]
            [ taskList lift model config.tasks goTo taskRoute
            ]
        ]


isHomePage : String -> Bool
isHomePage topbarTitle =
    if topbarTitle == "Home" then
        True

    else
        False


isTaskListNotEmpty : Maybe (List Task) -> Bool
isTaskListNotEmpty maybeTaskList =
    not (isTaskListEmpty maybeTaskList)


isTaskListEmpty : Maybe (List Task) -> Bool
isTaskListEmpty maybeTaskList =
    case maybeTaskList of
        Nothing ->
            True

        Just justTaskList ->
            List.isEmpty justTaskList


searchQuery : Maybe String -> Html m
searchQuery maybeQuery =
    case maybeQuery of
        Nothing ->
            span [] []

        Just query ->
            p [ class "mdc-typography--headline5" ] [ text ("Search result for '" ++ query ++ "'") ]


taskList : (Msg m -> m) -> Model m -> Maybe (List Task) -> (Route.Route -> m) -> (String -> Route.Route) -> Html m
taskList lift model tasks goTo taskRoute =
    case tasks of
        Nothing ->
            div [ class "" ] [ h3 [] [ text "No matches." ] ]

        Just justTaskList ->
            if List.isEmpty justTaskList then
                let
                    button =
                        Button.view (lift << Mdl) "ta-BlankSlate_Button" model.mdc [ cs "bs-Button", Button.ripple, Button.raised, Options.onClick (lift (GoTo Route.NewTask)) ] [ text "Create a task" ]
                in
                blankState "tasks" "racers-helmet" button

            else
                div [] (List.map (toCard lift goTo taskRoute) justTaskList)


toCard : (Msg m -> m) -> (Route.Route -> m) -> (String -> Route.Route) -> Task -> Html m
toCard lift goTo taskRoute task =
    span []
        [ div [ class "ta-TaskCard", onClick (goTo (taskRoute task.uuid)) ]
            [ text task.description
            ]
        , div [ class "ta-TaskCard__content mdc-card__actions mdc-card__action-icons" ]
            [ Icon.view [ Options.onClick (lift (MarkDone task.uuid)), Options.cs "mdc-card__action mdc-card__action--icon" ] "done"
            , Icon.view [ Options.onClick (lift (Delete task.uuid)), Options.cs "mdc-card__action mdc-card__action--icon" ] "delete"
            ]
        , hr [ class "mdc-list-divider" ] []
        ]
