module Pages.HomeTaskForm exposing
    ( config
    , initialModel
    )

import Data.Project as Project
import Data.Tag as Tag
import Data.Task as Task
import Date exposing (Date)
import Pages.TaskForm exposing (initEdit)
import Views.Helpers
    exposing
        ( ThreeColConfig
        , ThreeColMobileConfig
        , defaultThreeColConfig
        )
import Views.Model as ViewModel


config : ViewModel.Model m -> ThreeColConfig
config model =
    let
        c =
            defaultThreeColConfig model
    in
    { c
        | tasks = Just model.tasks
        , task = model.task
        , mobileConfig = ThreeColMobileConfig True True False
        , topbarTitle = "Home"
    }


initialModel :
    String
    -> List Project.Project
    -> List Tag.Tag
    -> List Task.Task
    -> Date
    -> Task.Task
    -> ViewModel.Model m
initialModel slug projects tags tasks now task =
    let
        model =
            ViewModel.defaultModel

        taskFormModel =
            initEdit task projects tags now
    in
    { model
        | slugOrQuery = slug
        , projects = projects
        , tags = tags
        , tasks = tasks
        , task = Just task
        , editTask = True
        , taskForm = Just taskFormModel
    }
