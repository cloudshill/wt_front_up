module Views.Helpers exposing
    ( ThreeColConfig
    , ThreeColMobileConfig
    , TwoColConfig
    , TwoColMobileConfig
    , ViewProject
    , ViewTag
    , buildUserTags
    , buildVirtualTags
    , defaultThreeColConfig
    , mapTaskCountToProject
    , mapTaskCountToTag
    , responsiveTopbar
    , secondaryColor
    )

import Data.Context exposing (Context)
import Data.Project exposing (Project)
import Data.Report exposing (Report)
import Data.Tag exposing (Tag)
import Data.Task exposing (Task)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Main.Route as Route exposing (Uuid)
import Views.Model as ViewModel


type alias ViewProject =
    { project : Project
    , taskCount : Int
    }


type alias ViewTag =
    { tag : Tag
    , taskCount : Int
    }


type alias ThreeColMobileConfig =
    { hideFirst : Bool
    , hideSecond : Bool
    , hideThird : Bool
    }


secondaryColor : String
secondaryColor =
    "#9b3d12"


type alias ThreeColConfig =
    { projects : List ViewProject
    , userTags : List ViewTag
    , virtualTags : List Tag
    , reports : List Report
    , tag : Maybe Tag
    , report : Maybe Report
    , tasks : Maybe (List Task)
    , task : Maybe Task
    , contexts : List Context
    , context : Maybe Context
    , project : Maybe Project
    , mobileConfig : ThreeColMobileConfig
    , fabRoute : Route.Route
    , taskRoute : Uuid -> Route.Route
    , taskFormRoute : Route.Route
    , topbarTitle : String
    , parentRoute : Route.Route
    , query : Maybe String
    }


buildUserTags : List Tag -> List Task -> List { tag : Tag, taskCount : Int }
buildUserTags tags tasks =
    let
        uTags =
            List.filter (\u -> u.virtual == False) tags
    in
    List.map
        (\tag ->
            ViewTag tag
                (mapTaskCountToTag tag tasks)
        )
        uTags
        |> List.filter (\tag -> tag.taskCount > 0)


buildVirtualTags : List Tag -> List Tag
buildVirtualTags tags =
    List.filter (\u -> u.virtual == True) tags


defaultThreeColConfig : ViewModel.Model m -> ThreeColConfig
defaultThreeColConfig model =
    let
        viewProjects =
            List.map
                (\project ->
                    ViewProject project
                        (mapTaskCountToProject project model.tasks)
                )
                model.projects
                |> List.filter (\project -> project.taskCount > 0)
    in
    { projects = viewProjects
    , userTags = buildUserTags model.tags model.tasks
    , virtualTags = buildVirtualTags model.tags
    , reports = model.reports
    , project = Nothing
    , tag = Nothing
    , report = Nothing
    , tasks = Nothing
    , task = Nothing
    , contexts = model.contexts
    , context = Nothing
    , mobileConfig = ThreeColMobileConfig True True True
    , fabRoute = Route.NewTask
    , taskRoute = Route.HomeTask
    , taskFormRoute = Route.Home
    , topbarTitle = "Default Title"
    , parentRoute = Route.Home
    , query = Nothing
    }


type alias TwoColMobileConfig =
    { hideFirst : Bool
    , hideSecond : Bool
    }


type alias TwoColConfig =
    { tasks : List Task
    , task : Maybe Task
    , mobileConfig : TwoColMobileConfig
    }


mapTasksToTag : Tag -> List Task -> List Task
mapTasksToTag tag tasks =
    tasks
        |> List.filter
            (\task ->
                List.any (\t -> t == tag.slug) task.tags
            )


mapTaskCountToTag : Tag -> List Task -> Int
mapTaskCountToTag tag tasks =
    mapTasksToTag tag tasks
        |> List.length


mapTasksToProject : Project -> List Task -> List Task
mapTasksToProject project tasks =
    tasks
        |> List.filter
            (\t ->
                String.startsWith
                    project.extendedName
                    (t.project |> Maybe.withDefault "")
            )


mapTaskCountToProject : Project -> List Task -> Int
mapTaskCountToProject project tasks =
    mapTasksToProject project tasks
        |> List.length


responsiveTopbar : Html msg -> Html msg -> List (Html msg)
responsiveTopbar tabletTopbar mobileTopbar =
    let
        tablet =
            div [ class "show-for-medium" ] [ tabletTopbar ]

        mobile =
            div [ class "show-for-small-only" ] [ mobileTopbar ]
    in
    [ tablet, mobile ]
