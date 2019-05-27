module Views.Tags.Tags exposing (Msg, update, view)

import Data.Tag exposing (Tag)
import Html exposing (Html, a, div, i, text)
import Html.Attributes exposing (class, classList)
import Html.Events as Html
import Main.Route as Route exposing (Route)
import Material exposing (Msg)
import Material.Button as Button
import Material.List as Lists
import Material.Options as Options exposing (cs, onClick)
import Pages.Shared.Page exposing (blankState)
import Views.Helpers exposing (ThreeColConfig, ViewTag)
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
            [ ( "cell grid-y xlarge-2 medium-9 g-ContentCol", True )
            , ( "show-for-xlarge", config.mobileConfig.hideFirst )
            ]
        ]
        [ div [ class "medium-12 cell cell-block-y" ]
            [ tagLists lift model config
            ]
        ]


isTagListNotEmpty : List ViewTag -> Bool
isTagListNotEmpty tags =
    not (isTagListEmpty tags)


isTagListEmpty : List ViewTag -> Bool
isTagListEmpty tags =
    List.isEmpty tags


toUserTagLi : (Msg m -> m) -> ViewTag -> Html m
toUserTagLi lift viewTag =
    let
        tag =
            viewTag.tag
    in
    Lists.li
        [ cs "tl-TagListItem", Options.onClick (lift (GoTo (Route.Tag tag.slug))) ]
        [ Html.text tag.name
        , Lists.meta [] [ text (toString viewTag.taskCount) ]
        ]


toVirtualTagLi : (Msg m -> m) -> Tag -> Html m
toVirtualTagLi lift virtualTag =
    Lists.li
        [ cs "tl-TagListItem", Options.onClick (lift (GoTo (Route.VirtualTag virtualTag.slug))) ]
        [ Html.text virtualTag.name
        ]


userTagList : (Msg m -> m) -> Model m -> List ViewTag -> List (Html m)
userTagList lift model userTags =
    if List.isEmpty userTags then
        let
            button =
                Button.view (lift << Mdc)
                    "tl-BlankSlate_Button"
                    model.mdc
                    [ cs "tl-BlankUserTags_Button"
                    , Button.ripple
                    , Options.onClick (lift (GoTo Route.NewTask))
                    ]
                    [ text "Create a task" ]
        in
        [ div [ class "mdc-typography--body2 tl-BlankUserTags" ]
            [ text "You have no user defined tags. Create a new task with a tag, then you can view the tag here." ]
        , button
        ]

    else
        List.map (toUserTagLi lift) userTags


tagLists : (Msg m -> m) -> Model m -> ThreeColConfig -> Html m
tagLists lift model config =
    Lists.group [ cs "" ]
        [ Lists.subheader [] [ Html.text "Tags" ]
        , div
            [ classList
                [ ( "g-FirstColList", True )
                , ( "tl-BlankSlateContainer", List.isEmpty config.userTags )
                ]
            ]
            (userTagList lift model config.userTags)
        , Lists.groupDivider [] []
        , Lists.subheader [] [ Html.text "Virtual Tags" ]
        , Lists.ul
            [ cs "tl-VirtualTagList g-FirstColList"
            ]
            (List.map (toVirtualTagLi lift) config.virtualTags)
        ]
