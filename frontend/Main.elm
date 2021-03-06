module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (srcdoc)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Time exposing (Time)


-- MODEL


type alias Model =
    { fmt : Maybe String
    , inputData : String
    , tickCount : Int
    , outputData : Maybe String
    , knownFormats : Maybe (List String)
    , hasChanges : Bool
    }


mkModel : Model
mkModel =
    { fmt = Just "markdown"
    , inputData = ""
    , tickCount = 0
    , outputData = Nothing
    , knownFormats = Nothing
    , hasChanges = False
    }


init : ( Model, Cmd Msg )
init =
    ( mkModel, fetchKnownFormats )



-- UPDATE


type Msg
    = SelectFormat String
    | UpdateContent String
    | Tick Time
    | FormatUpdateMsg (Result Http.Error (List String))
    | PreviewUpdateMsg (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectFormat f ->
            ( { model | fmt = Just f, hasChanges = True }
            , updatePreview model.inputData f
            )

        UpdateContent newContent ->
            ( { model | tickCount = 0, inputData = newContent, hasChanges = True }
            , Cmd.none
            )

        Tick t ->
            let
                newCnt =
                    model.tickCount + 1

                f =
                    if newCnt > 1 && (String.isEmpty model.inputData == False) && model.hasChanges then
                        updatePreview model.inputData (Maybe.withDefault "markdown" model.fmt)
                    else
                        Cmd.none
            in
            ( { model | tickCount = newCnt }
            , f
            )

        FormatUpdateMsg (Err error) ->
            ( { model | knownFormats = Nothing }
            , fetchKnownFormats
            )

        FormatUpdateMsg (Ok lst) ->
            ( { model | knownFormats = Just lst }
            , Cmd.none
            )

        PreviewUpdateMsg (Err error) ->
            let
                msg =
                    stringifyErr error
            in
            ( { model | tickCount = 0, hasChanges = False, outputData = Just msg }
            , Cmd.none
            )

        PreviewUpdateMsg (Ok innerHTML) ->
            ( { model | tickCount = 0, outputData = Just innerHTML, hasChanges = False }
            , Cmd.none
            )


stringifyErr : Http.Error -> String
stringifyErr e =
    case e of
        Http.BadUrl s ->
            "Bad URL" ++ s

        Http.Timeout ->
            "Request Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus _ ->
            "Bad Status"

        Http.BadPayload _ _ ->
            "Bad Payload"


updatePreview : String -> String -> Cmd Msg
updatePreview s fmt =
    newRenderRequest s fmt
        |> Http.send PreviewUpdateMsg


newRenderRequest : String -> String -> Http.Request String
newRenderRequest s fmt =
    Http.request
        { method = "POST"
        , headers = []
        , url = previewURL fmt
        , body = Http.stringBody "" s
        , expect = Http.expectString
        , timeout = Nothing
        , withCredentials = False
        }


fetchKnownFormats : Cmd Msg
fetchKnownFormats =
    Http.get formatListURL (Decode.list Decode.string)
        |> Http.send FormatUpdateMsg



-- VIEW


view : Model -> Html Msg
view m =
    div [] [ formatDiv m, inputBox, outputBox m ]


formatButton : String -> Html Msg
formatButton s =
    label [ Html.Attributes.style [ ( "padding", "20px" ) ] ]
        [ input
            [ Html.Attributes.type_ "radio"
            , Html.Attributes.name "format"
            , onClick (SelectFormat s)
            ]
            []
        , text s
        ]


formatDiv : Model -> Html Msg
formatDiv m =
    let
        fmts =
            m.knownFormats

        btns =
            List.map formatButton (Maybe.withDefault [ "markdown" ] fmts)

        warning =
            if fmts == Nothing then
                div [] [ text "Cannot fetch list of supported formats. Defaulting to 'markdown'" ]
            else
                div [] []

        s =
            [ warning ] ++ btns
    in
    div []
        [ h2 [] [ text "Select Format to Preview" ]
        , div [] s
        ]


inputBox : Html Msg
inputBox =
    div []
        [ h2 [] [ text "Input" ]
        , textarea [ onInput UpdateContent ] []
        ]


outputBox : Model -> Html Msg
outputBox m =
    let
        innerHTML =
            Maybe.withDefault "" m.outputData
    in
    div []
        [ div [] [ h2 [] [ text "Preview" ] ]
        , iframe [ srcdoc innerHTML ] []
        ]



-- MAIN


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (200 * Time.millisecond) Tick


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UTIL


formatListURL : String
formatListURL =
    "/supportedformats"


previewURL : String -> String
previewURL fmt =
    "/render?format=" ++ fmt


defaultFormatList : List String
defaultFormatList =
    [ "markdown" ]
