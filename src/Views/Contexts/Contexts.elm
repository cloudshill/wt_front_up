module Views.Contexts.Contexts exposing
    ( ExternalMsg(..)
    , Msg
    , update
    , view
    )

import Data.Context exposing (Context)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Main.Route as Route exposing (Route)
import Main.Util exposing ((=>))
import Material
import Material.Button as Button
import Material.List as Lists
import Material.Options as Options exposing (cs, when)
import Material.Switch as Switch
import Pages.Shared.Page exposing (FixedAdjust(..), blankState)
import SelectList
import Tuple exposing (first, second)
import Views.Helpers exposing (ThreeColConfig)
import Views.Model as ViewModel


type Msg m
    = Mdl (Material.Msg m)
    | ToggleSwitch Int
    | GoTo Route


type ExternalMsg
    = NoOp
    | ChangeContext String


mapContexts : List Context -> List ( Int, String )
mapContexts contexts =
    let
        userContexts =
            List.drop 1 contexts

        imap idx context =
            ( idx + 1, context.name )
    in
    List.indexedMap imap userContexts


update : (Msg m -> m) -> Msg m -> ViewModel.Model m -> ( ( ViewModel.Model m, Cmd m ), ExternalMsg )
update lift msg model =
    case msg of
        Mdl msg_ ->
            Material.update (Mdl >> lift) msg_ model => NoOp

        GoTo route ->
            model => Route.newUrl route => NoOp

        ToggleSwitch idx ->
            let
                a =
                    List.map .name model.contexts

                context =
                    List.drop idx a |> List.head
            in
            { model
                | activeContext =
                    SelectList.select (\u -> u == idx)
                        model.activeContext
            }
                => Cmd.none
                => ChangeContext (Maybe.withDefault "none" context)


view :
    (Msg m -> m)
    -> ViewModel.Model m
    -> ThreeColConfig
    -> Html m
view lift model config =
    div
        [ classList
            [ ( "cell grid-y auto", True )
            , ( "show-for-xlarge", config.mobileConfig.hideSecond )
            , ( "g-ContentCol", isContextListNotEmpty config.contexts )
            ]
        ]
        [ div [ class "medium-12 cell cell-block-y" ]
            [ content lift model config ]
        ]


isContextListNotEmpty : List Context -> Bool
isContextListNotEmpty contexts =
    not (isContextListEmpty contexts)


isContextListEmpty : List Context -> Bool
isContextListEmpty contexts =
    List.isEmpty contexts


content : (Msg m -> m) -> ViewModel.Model m -> ThreeColConfig -> Html m
content lift model config =
    if isContextListEmpty config.contexts then
        let
            button =
                Button.view (lift << Mdl) "ct-BlankSlate_Button" model.mdc [ cs "bs-Button", Button.ripple, Button.raised, Options.onClick (lift (GoTo Route.NewContext)) ] [ text "Create a context" ]
        in
        blankState "contexts" "wheelbarrow" button

    else
        contextList lift model config


contextList : (Msg m -> m) -> ViewModel.Model m -> ThreeColConfig -> Html m
contextList lift model config =
    Lists.ul [ cs "g-FirstColList" ]
        ([ contentListItem lift model "none" 0
         , Lists.divider [ cs "ct-ListDivider" ] []
         ]
            ++ List.map (\c -> contentListItem lift model (second c) (first c))
                (mapContexts config.contexts)
        )


contentListItem : (Msg m -> m) -> ViewModel.Model m -> String -> Int -> Html m
contentListItem lift model name idx =
    Lists.li [ cs "ct-ListItem" ]
        [ span [ class "ct-ListItem__span", onClick (lift (GoTo (Route.Context name))) ] [ div [] [ text name ] ]
        , Lists.meta []
            [ Switch.view (Mdl >> lift)
                ("ct-ListItem__switch"
                    ++ toString idx
                )
                model.mdc
                [ Options.onClick (lift (ToggleSwitch idx))
                , Switch.on
                    |> when
                        ((model.activeContext
                            |> SelectList.selected
                         )
                            == idx
                        )
                ]
                []
            ]
        ]
