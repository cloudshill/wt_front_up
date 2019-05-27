module Pages.Shared.Toolbars.Searchbar.Searchbar exposing (render)

import Html exposing (Html, form, input)
import Html.Attributes exposing (class, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Material
import Material.Options exposing (cs)
import Material.Toolbar as Toolbar
import Views.LoadingBar.LoadingBar as LoadingBar


render :
    (Material.Msg m -> m)
    -> Material.Index
    -> Material.Model m
    -> Bool
    -> Html m
    -> (String -> m)
    -> m
    -> Html m
render lift idx model isLoading backArrow setSearchQuery submitSearch =
    Toolbar.view lift
        idx
        model
        [ Toolbar.fixed
        , cs "st-Toolbar"
        ]
        [ Toolbar.row []
            [ Toolbar.section [ Toolbar.alignStart ]
                [ backArrow
                , form [ onSubmit submitSearch, class "st-searchForm" ]
                    [ input
                        [ onInput setSearchQuery
                        , placeholder "Search"
                        , type_ "Search"
                        ]
                        []
                    ]
                ]
            ]
        , LoadingBar.render isLoading ""
        ]
