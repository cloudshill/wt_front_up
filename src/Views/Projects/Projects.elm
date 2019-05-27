module Views.Projects.Projects exposing (Msg, update, view)

import Html exposing (Html, a, div, h3, i, img, text)
import Html.Attributes exposing (class, classList, src)
import Main.Route as Route exposing (Route)
import Material exposing (Msg)
import Material.Button as Button
import Material.List as Lists
import Material.Options exposing (cs, onClick)
import Pages.Shared.Page exposing (blankState)
import Views.Helpers exposing (ThreeColConfig, ViewProject)
import Views.Model exposing (Model)


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
            , ( "g-ContentCol", isProjectListNotEmpty config.projects )
            , ( "xlarge-2 medium-9", isProjectListNotEmpty config.projects )
            , ( "auto", isProjectListEmpty config.projects )
            ]
        ]
        [ div [ class "medium-12 cell cell-block-y" ]
            [ projectsList lift model config.projects
            ]
        ]


isProjectListNotEmpty : List ViewProject -> Bool
isProjectListNotEmpty projects =
    not (isProjectListEmpty projects)


isProjectListEmpty : List ViewProject -> Bool
isProjectListEmpty projects =
    List.isEmpty projects


toLi : (Msg m -> m) -> ViewProject -> Html m
toLi lift viewProject =
    Lists.li
        [ cs ("pl-Project pl-Project_Link-nest" ++ viewProject.project.nestingLevel)
        , onClick (lift (GoTo (Route.Project viewProject.project.slug)))
        ]
        [ Html.text viewProject.project.name
        , Lists.meta [] [ text (toString viewProject.taskCount) ]
        ]


projectsList : (Msg m -> m) -> Model m -> List ViewProject -> Html m
projectsList lift model viewProjects =
    if isProjectListEmpty viewProjects then
        let
            button =
                Button.view (lift << Mdc) "pl-BlankSlate_Button" model.mdc [ cs "bs-Button", Button.ripple, Button.raised, onClick (lift (GoTo Route.NewTask)) ] [ text "Create a task" ]
        in
        blankState "projects" "weathercock" button

    else
        Lists.ul [ cs "g-FirstColList" ] (List.map (toLi lift) viewProjects)
