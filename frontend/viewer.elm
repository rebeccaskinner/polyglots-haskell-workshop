import Html exposing (..)

type DocumentFormat = LaTeX | MediaWiki | Markdown | Org

type Msg = SelectFormat DocumentFormat | UpdateContent String | Render

type alias Model =
    { fmt : DocumentFormat
    , inputData : String
    , outputData : Maybe String
    }

mkModel : Model
mkModel = Model Markdown "" Nothing

main =  Html.beginnerProgram { model = mkModel, view = view, update = update }

-- UPDATE

update : Msg -> Model -> Model
update msg model = model

-- VIEW
view _ = text "foo"
