module Views.TaskForm.ProjectPicker exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import Data.Project as Project
import Fuzzy
import Html exposing (Html, div, i, text)
import Html.Attributes exposing (class)
import Material
import Material.List as Lists
import Material.Options as Options exposing (cs)
import Views.TaskForm.Toolbars.ProjectPicker as Toolbar


type alias Model m =
    { mdc : Material.Model m
    , needle : String
    , haystack : List String
    , caseInsensitive : Bool
    , separators : String
    }


initialModel : List Project.Project -> Model m
initialModel projects =
    let
        projectNames =
            List.map (\p -> p.extendedName) projects
    in
    { mdc = Material.defaultModel
    , needle = ""
    , separators = ""
    , caseInsensitive = True
    , haystack = projectNames
    }


type Msg m
    = Mdl (Material.Msg m)
    | SetNeedle String
    | ClearNeedle


update : (Msg m -> m) -> Msg m -> Model m -> ( Model m, Cmd m )
update lift msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model

        SetNeedle needle ->
            ( { model | needle = needle }, Cmd.none )

        ClearNeedle ->
            ( { model | needle = "" }, Cmd.none )


view : (Msg m -> m) -> Model m -> m -> (String -> m) -> Html m
view lift model hideProjectPicker setProject =
    div [ class "tf-Picker_Toobar-fixedadjust" ]
        [ Toolbar.render
            (Mdl >> lift)
            "tf-ProjectPicker_Toolbar"
            model.mdc
            hideProjectPicker
            (lift << SetNeedle)
            (lift ClearNeedle)
            model.needle
            setProject
        , viewHayStack model setProject
        ]


viewElement : (String -> m) -> ( Fuzzy.Result, String ) -> Html m
viewElement setProject ( _, item ) =
    Lists.li [ Options.cs "tf-Picker_ListItem", Options.onClick (setProject item) ]
        [ Lists.graphic [] [ i [ class "material-icons" ] [ text "work" ] ]
        , Html.text item
        ]


viewHayStack : Model m -> (String -> m) -> Html m
viewHayStack model setProject =
    let
        processCase item =
            if model.caseInsensitive then
                String.toLower item

            else
                item

        separators =
            String.toList (processCase model.separators)
                |> List.map String.fromChar

        needle =
            processCase model.needle

        haystack =
            if model.needle == "" then
                model.haystack

            else
                model.needle :: model.haystack

        scoredHays =
            haystack
                |> List.map
                    (\hay ->
                        ( Fuzzy.match []
                            separators
                            needle
                            (processCase hay)
                        , hay
                        )
                    )

        filteredHays =
            List.filter (\e -> (Tuple.first e).score < 10000) scoredHays

        sortedHays =
            if model.needle == "" then
                List.sortBy Tuple.second filteredHays

            else
                List.sortBy (\e -> Tuple.first e |> .score) filteredHays
    in
    Lists.ul [ cs "lists-display", Lists.avatarList ]
        (sortedHays
            |> List.map (viewElement setProject)
        )
