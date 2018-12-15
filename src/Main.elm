module Main exposing (main)

import Browser exposing (Document)
import Data exposing (Data)
import Debug
import Html
import Maybe.Extra as Maybe
import Url exposing (Url)
import Url.Parser as Url exposing ((<?>))
import Url.Parser.Query as Query


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = always NoMsg
        , onUrlChange = always NoMsg
        }


type alias Model =
    { data : Maybe Data
    }


init : () -> Url -> key -> ( Model, Cmd Msg )
init _ url _ =
    ( { data = Nothing }
    , Maybe.unwrap Cmd.none (Cmd.map DataMsg << Data.fetch) (parseDataUrl url)
    )


type Msg
    = DataMsg Data.Msg
    | NoMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataMsg msg2 ->
            ( { model | data = Data.update msg2 }, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Visualize Activities"
    , body = [ Html.text (Debug.toString model) ]
    }


parseDataUrl : Url -> Maybe String
parseDataUrl url =
    let
        parser =
            Url.top <?> Query.string "data"
    in
    Maybe.join <| Url.parse parser { url | path = "" }
