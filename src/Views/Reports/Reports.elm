module Views.Reports.Reports exposing (Msg, update, view)

import Data.Report exposing (Report)
import Html exposing (Html, a, div, i, text)
import Html.Attributes exposing (class, classList)
import Main.Route as Route exposing (Route)
import Material exposing (Msg)
import Material.Button as Button
import Material.List as Lists
import Material.Options exposing (cs, onClick)
import Pages.Shared.Page exposing (blankState)
import Views.Helpers exposing (ThreeColConfig)
import Views.Model exposing (Model)
import Views.Tasks.Tasks exposing (isTaskListEmpty, isTaskListNotEmpty)


type Msg m
    = Mdc (Material.Msg m)
    | GoTo Route


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


view : (Msg m -> m) -> Model m -> ThreeColConfig -> Html m
view lift model config =
    div
        [ classList
            [ ( "cell grid-y", True )
            , ( "show-for-xlarge", config.mobileConfig.hideFirst )
            , ( "g-ContentCol", True )
            , ( "xlarge-2 medium-9", True )
            ]
        ]
        [ div [ class "medium-12 cell cell-block-y" ]
            [ reportList config
            ]
        ]


reportList : ThreeColConfig -> Html m
reportList config =
    Lists.group [ cs "lists-display" ]
        [ Lists.ul
            [ Lists.twoLine
            , cs "tl-VirtualTagList"
            ]
            (List.map toReportLi config.reports)
        ]


toReportLi : Report -> Html m
toReportLi report =
    Lists.li [ cs "tl_VirtualTag" ]
        [ a [ Route.href (Route.Report report.slug) ]
            [ Lists.text []
                [ Html.text report.name
                , Lists.secondaryText []
                    [ Html.text report.description
                    ]
                ]
            , Lists.meta [ cs "gray-bg" ] []
            ]
        ]
