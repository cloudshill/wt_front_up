module Views.Model exposing (Model, defaultActiveContext, defaultModel)

import Data.Context as Context exposing (Context)
import Data.Project exposing (Project)
import Data.Report exposing (Report)
import Data.Setting exposing (Setting)
import Data.Tag exposing (Tag)
import Data.Task exposing (Task)
import Data.User exposing (Username)
import List.Extra
import Material
import SelectList exposing (SelectList)
import Views.TaskForm.Model as TaskFormModel exposing (Model)


defaultActiveContext : SelectList Int
defaultActiveContext =
    mapContextsToSelectList []


mapContextsToSelectList : List Context.Context -> SelectList Int
mapContextsToSelectList contexts =
    let
        l =
            SelectList.fromLists [] 0 (List.range 0 (List.length contexts - 1))

        activeContext =
            Context.findActiveContext contexts

        idx =
            List.Extra.findIndex (\c -> c == activeContext) contexts |> Maybe.withDefault 0
    in
    SelectList.select (\u -> u == idx) l


type alias Model m =
    { mdc : Material.Model m
    , errors : List String
    , slugOrQuery : String
    , projects : List Project
    , tags : List Tag
    , reports : List Report
    , contexts : List Context
    , context : Maybe Context
    , activeContext : SelectList Int
    , settings : List Setting
    , tasks : List Task
    , task : Maybe Task
    , editTask : Bool
    , taskForm : Maybe (TaskFormModel.Model m)
    , username : String
    , requestedTasks : List Task
    }


defaultModel : Model m
defaultModel =
    { mdc = Material.defaultModel
    , errors = []
    , slugOrQuery = ""
    , projects = []
    , tags = []
    , reports = []
    , settings = []
    , contexts = []
    , context = Nothing
    , activeContext = defaultActiveContext
    , tasks = []
    , task = Nothing
    , editTask = False
    , taskForm = Nothing
    , username = ""
    , requestedTasks = []
    }
