module Pages.Bucket.Bucket exposing
    ( ExternalMsg(..)
    , Model
    , Msg
    , RequestType(..)
    , init
    , initialModelByProject
    , initialModelByTag
    , update
    , view
    )

import Data.Project as Project
import Data.Session as Session exposing (Session)
import Data.Task as WTask
import Html exposing (Html, div, hr, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Main.Route as Route exposing (Route)
import Main.Util as Util exposing ((=>), pair)
import Material exposing (Msg)
import Material.Icon as Icon
import Material.Options as Options
import Pages.Shared.Page as Page
import Requests.Bucket.Task
import Requests.Report.Task
import Requests.Task
import Task
import Views.Errored.Errored as Errored exposing (pageLoadError)


type alias Model m =
    { mdc : Material.Model m
    , errors : List String
    , slugOrQuery : String
    , tasks : List WTask.Task
    }


type Msg m
    = Mdl (Material.Msg m)
    | GoTo Route
    | MarkDone String
    | Delete String
    | TaskMarkedDone String (Result Http.Error ())
    | TaskDeleted String (Result Http.Error ())


type ExternalMsg
    = NoOp
    | RemoveTask String String


type RequestType
    = Report
    | VirtualTag
    | Search


initialModelByTag : String -> List WTask.Task -> Model m
initialModelByTag slug tasks =
    let
        tagTasks =
            tasks
                |> List.filter
                    (\task ->
                        List.any (\tag -> tag == slug) task.tags
                    )
    in
    Model Material.defaultModel [] slug tagTasks


initialModelByProject :
    String
    -> List Project.Project
    -> List WTask.Task
    -> Model m
initialModelByProject slug projects tasks =
    let
        projectName =
            projects
                |> List.filter (\p -> p.slug == slug)
                |> List.map (\p -> p.extendedName)
                |> List.head
                |> Maybe.withDefault ""

        projectTasks =
            tasks
                |> List.filter
                    (\t ->
                        String.startsWith
                            projectName
                            (t.project |> Maybe.withDefault "")
                    )
    in
    Model Material.defaultModel [] slug projectTasks


init : Session -> RequestType -> String -> Task.Task PageLoadError (Model m)
init session requestType slugOrQuery =
    let
        maybeAuthToken =
            Maybe.map .token session.user

        loadTasks =
            case requestType of
                Report ->
                    Requests.Report.Task.list maybeAuthToken slugOrQuery
                        |> Http.toTask

                VirtualTag ->
                    Requests.Bucket.Task.list maybeAuthToken
                        "virtual_tags"
                        slugOrQuery
                        |> Http.toTask

                Search ->
                    Requests.Bucket.Task.search maybeAuthToken slugOrQuery
                        |> Http.toTask

        handleLoadError _ =
            "There was a problem with loading bucket."
                |> pageLoadError Page.Home
    in
    Task.map (Model Material.defaultModel [] slugOrQuery) loadTasks
        |> Task.mapError handleLoadError


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

        MarkDone uuid ->
            let
                cmdFromAuth authToken =
                    Requests.Task.markDone authToken uuid
                        |> Http.send (lift << TaskMarkedDone uuid)
            in
            session
                |> Session.attempt "marktaskdone" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors model)
                => NoOp

        Delete uuid ->
            let
                cmdFromAuth authToken =
                    Requests.Task.delete authToken uuid
                        |> Http.send (lift << TaskDeleted uuid)
            in
            session
                |> Session.attempt "deletetask" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors model)
                => NoOp

        TaskDeleted uuid (Ok ()) ->
            Route.Home
                |> Route.newUrl
                |> pair model
                => RemoveTask uuid "Task deleted."

        TaskDeleted _ (Err _) ->
            ( { model
                | errors =
                    model.errors
                        ++ [ "Server error while trying to set context." ]
              }
            , Cmd.none
            )
                => NoOp

        TaskMarkedDone uuid (Ok ()) ->
            Route.Home
                |> Route.newUrl
                |> pair model
                => RemoveTask uuid "Task marked done."

        TaskMarkedDone _ (Err _) ->
            ( { model
                | errors =
                    model.errors
                        ++ [ "Server error while trying to set context." ]
              }
            , Cmd.none
            )
                => NoOp


view :
    (Msg m -> m)
    -> Model m
    ->
        { topbar : String -> Maybe (Html m) -> Html m
        , bottombar : Route.Route -> Html m
        , snackbar : Route.Route -> Html m
        }
    -> Html m
    -> Route.Route
    -> Html m
view lift model components backArrow parentRoute =
    let
        topbar =
            components.topbar model.slugOrQuery (Just backArrow)

        bottombar =
            components.bottombar parentRoute

        snackbar =
            components.snackbar Route.NewTask

        pageContent =
            content lift model
    in
    Page.frame topbar pageContent bottombar snackbar Page.FixedAdjustOn


content : (Msg m -> m) -> Model m -> Html m
content lift model =
    div [ class "grid-container" ]
        [ div [ class "grid-x" ]
            [ div [ class "bucket large-3 hide-for-small-only cell" ]
                []
            , div [ class "large-6 cell" ]
                [ bucketList lift model.tasks
                ]
            , div [ class "large-3 cell hide-for-small-only" ]
                []
            ]
        , div [ class "grid-x" ]
            [ div [ class "cell" ]
                []
            ]
        ]


toCard : (Msg m -> m) -> WTask.Task -> Html m
toCard lift task =
    span []
        [ div [ class "ho-TaskCard", onClick (lift (GoTo (Route.Task task.uuid))) ]
            [ text task.description
            ]
        , div [ class "mdc-card__actions mdc-card__action-icons" ]
            [ Icon.view [ Options.onClick (lift (MarkDone task.uuid)), Options.cs "mdc-card__action mdc-card__action--icon" ] "done"
            , Icon.view [ Options.onClick (lift (Delete task.uuid)), Options.cs "mdc-card__action mdc-card__action--icon" ] "delete"
            ]
        , hr [ class "mdc-list-divider" ] []
        ]



-- toCard : (Msg m -> m) -> WTask.Task -> Html m
-- toCard lift task =
--     Card.view [ Options.onClick (lift (GoTo (Route.Task task.uuid))) ]
--         [ Card.primary []
--             [ Card.title []
--                 [ text task.description ]
--             ]
--         ]


bucketList : (Msg m -> m) -> List WTask.Task -> Html m
bucketList lift tasks =
    if List.isEmpty tasks then
        div [ class "bu-Tasks-nomatches" ] [ text "No matches." ]

    else
        div [ class "large-6 cell" ] (List.map (toCard lift) tasks)
