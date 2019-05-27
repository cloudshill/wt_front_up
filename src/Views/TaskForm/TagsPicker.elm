module Views.TaskForm.TagsPicker exposing
    ( Model
    , Msg
    , initialModel
    , update
    , view
    )

import Data.Tag as Tag
import Fuzzy
import Html exposing (Html, div, hr, i, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Material
import Material.List as Lists
import Material.Options as Options exposing (cs)
import Set
import Views.TaskForm.Toolbars.TagsPicker as Toolbar


type alias Model m =
    { mdc : Material.Model m
    , needle : String
    , haystack : List String
    , caseInsensitive : Bool
    , separators : String
    }


initialModel : List Tag.Tag -> Model m
initialModel tags =
    let
        tagNames =
            tags
                |> List.filter (\t -> t.virtual == False)
                |> List.map (\t -> t.name)
    in
    { mdc = Material.defaultModel
    , needle = ""
    , separators = ""
    , caseInsensitive = True
    , haystack = tagNames
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
            ( { model | needle = String.filter (\n -> n /= ' ') needle }, Cmd.none )

        ClearNeedle ->
            ( { model | needle = "" }, Cmd.none )



-- if needle is clear and tags greater than 1 show selected tags


view :
    (Msg m -> m)
    -> Model m
    -> m
    -> (String -> m)
    -> (String -> m)
    -> List String
    -> Html m
view lift model hideTagPicker addTag removeTag selectedTags =
    let
        selectedSet =
            Set.fromList selectedTags

        haystackSet =
            Set.fromList model.haystack

        availableSet =
            Set.diff haystackSet selectedSet

        availableList =
            Set.toList availableSet

        content =
            if not (List.isEmpty selectedTags) then
                div []
                    [ viewTagsAdded (List.sort selectedTags) removeTag
                    , hr [] []
                    , viewHayStack model availableList addTag
                    ]

            else
                viewHayStack model model.haystack addTag
    in
    div [ class "tf-Picker_Toolbar-fixedadjust" ]
        [ Toolbar.render
            (Mdl >> lift)
            "tf-TagsPicker_Toolbar"
            model.mdc
            hideTagPicker
            (lift << SetNeedle)
            (lift ClearNeedle)
            model.needle
            addTag
        , content
        ]


viewTagsAdded : List String -> (String -> m) -> Html m
viewTagsAdded tags removeTag =
    Lists.group []
        [ Lists.ul [ cs "lists-display", Lists.avatarList ]
            (tags
                |> List.map (viewTag removeTag)
            )
        ]


viewTag : (String -> m) -> String -> Html m
viewTag removeTag tag =
    Lists.li []
        [ Lists.graphic [ Options.cs "gray-bg" ] [ i [ class "material-icons" ] [ text "label" ] ]
        , Html.text tag
        , Lists.meta []
            [ i
                [ onClick (removeTag tag)
                , class "material-icons"
                ]
                [ text "clear" ]
            ]
        ]


viewElement : (String -> m) -> ( Fuzzy.Result, String ) -> Html m
viewElement addTag ( _, item ) =
    Lists.li [ Options.cs "tf-TagsPicker_UnselectedItem", Options.onClick (addTag item) ]
        [ Lists.graphic [ Options.cs "gray-bg" ] [ i [ class "material-icons" ] [ text "label" ] ]
        , Html.text item
        ]


viewHayStack : Model m -> List String -> (String -> m) -> Html m
viewHayStack model availableTags addTag =
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
                availableTags

            else
                model.needle :: availableTags

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
            |> List.map (viewElement addTag)
        )
