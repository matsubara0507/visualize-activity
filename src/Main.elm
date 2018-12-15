module Main exposing (main)

import Array
import Axis
import Browser exposing (Document)
import Color exposing (Color)
import Data exposing (Data)
import Debug
import Dict exposing (Dict)
import Dict.Extra as Dict
import Html exposing (Html)
import Html.Attributes as Html
import List.Extra as List
import Maybe.Extra as Maybe
import Scale
import Scale.Color as Scale
import Tuple
import TypedSvg as Svg
import TypedSvg.Attributes as SvgAttr
import TypedSvg.Attributes.InPx as SvgAttrPx
import TypedSvg.Core as Svg exposing (Svg)
import TypedSvg.Types as Svg
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
    , title : String
    , width : Float
    , height : Float
    , padding : Float
    , size : Maybe Int
    , debug : Bool
    }


init : () -> Url -> key -> ( Model, Cmd Msg )
init _ url _ =
    ( { data = Nothing
      , title = mkTitle url
      , width = 900
      , height = 450
      , padding = 30
      , size = parseSize url
      , debug = parseDebug url
      }
    , Maybe.unwrap Cmd.none (Cmd.map DataMsg << Data.fetch) (parseDataUrl url)
    )


mkTitle : Url -> String
mkTitle url =
    let
        name =
            Maybe.unwrap "Your" (\s -> s ++ "'s") (parseName url)

        file =
            Maybe.withDefault "" (parseFileName url)
    in
    name ++ " Outputs Chart: " ++ file


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
    { title = model.title
    , body =
        [ Html.h2 [ Html.style "text-align" "center" ] [ Html.text model.title ]
        , Html.div
            [ Html.style "position" "absolute"
            , Html.style "margin-left" "20px"
            ]
            (List.reverse <|
                Dict.values <|
                    Dict.map toColorPad <|
                        Maybe.unwrap Dict.empty colors model.data
            )
        , Svg.svg
            [ SvgAttr.viewBox 0 0 model.width model.height ]
            [ Svg.g
                [ SvgAttr.transform
                    [ Svg.Translate (model.padding - 1) (model.height - model.padding) ]
                ]
                [ xAxis model ]
            , Svg.g
                [ SvgAttr.transform
                    [ Svg.Translate model.padding model.padding ]
                , SvgAttr.class [ "series" ]
                ]
                (Maybe.unwrap [] (Dict.values << Dict.map (column model)) model.data)
            ]
        , if model.debug then
            Html.text (Debug.toString model)

          else
            Html.div [] []
        ]
    }


toColorPad : String -> Color -> Html msg
toColorPad key val =
    Html.div
        [ Html.style "border-left" ("10px solid " ++ Color.toCssString val)
        , Html.style "padding" "5px"
        ]
        [ Html.text key ]


xAxis : Model -> Svg Msg
xAxis model =
    Axis.bottom [] <| Scale.toRenderable toMonth (xScale model)


xScale : Model -> Scale.BandScale String
xScale model =
    let
        default =
            Scale.defaultBandConfig
    in
    Maybe.unwrap [] Dict.keys model.data
        |> Scale.band
            { default | paddingInner = 0.1, paddingOuter = 0.2 }
            ( 0, model.width - 2 * model.padding )


yScale : Model -> Scale.ContinuousScale Float
yScale model =
    let
        maxSize =
            Maybe.unwrap [] Dict.values model.data
                |> List.map List.length
                |> List.maximum
                |> Maybe.withDefault 5
    in
    Scale.linear
        ( model.height - 2 * model.padding, 0 )
        ( 0, toFloat <| Maybe.withDefault maxSize model.size )


column : Model -> String -> List Data.Output -> Svg Msg
column model key outputs =
    let
        scale =
            { x = xScale model, y = yScale model }

        value =
            toFloat <| List.length outputs

        values =
            Dict.groupBy .media outputs
                |> Dict.map (always (toFloat << List.length))
    in
    Svg.g [ SvgAttr.class [ "column" ] ]
        [ Svg.g [] <|
            List.reverse <|
                Tuple.second <|
                    List.mapAccuml (rectValue model scale key) 0 (Dict.toList values)
        , Svg.text_
            [ SvgAttrPx.x <| Scale.convert (Scale.toRenderable toMonth scale.x) key
            , SvgAttrPx.y <| Scale.convert scale.y value - 5
            , SvgAttr.textAnchor Svg.AnchorMiddle
            ]
            [ Svg.text <| String.fromFloat value ]
        ]


rectValue :
    Model
    -> { x : Scale.BandScale String, y : Scale.ContinuousScale Float }
    -> String
    -> Float
    -> ( String, Float )
    -> ( Float, Svg Msg )
rectValue model scale month acc ( key, value ) =
    ( acc + value
    , Svg.rect
        [ Html.id key
        , SvgAttr.fill <|
            Maybe.unwrap Svg.FillNone Svg.Fill <|
                Maybe.andThen (Dict.get key) <|
                    Maybe.map colors model.data
        , SvgAttrPx.x <| Scale.convert scale.x month
        , SvgAttrPx.y <| Scale.convert scale.y (acc + value)
        , SvgAttrPx.width <| Scale.bandwidth scale.x
        , SvgAttrPx.height <|
            model.height
                - Scale.convert scale.y (acc + value)
                - (2 * model.padding)
        ]
        []
    )


parseDataUrl : Url -> Maybe String
parseDataUrl url =
    Url.parse (Url.top <?> Query.string "data") { url | path = "" }
        |> Maybe.join


parseFileName : Url -> Maybe String
parseFileName url =
    parseDataUrl url
        |> Maybe.andThen Url.fromString
        |> Maybe.map (String.split "/" << .path)
        |> Maybe.andThen (List.head << List.reverse)
        |> Maybe.map (String.split ".")
        |> Maybe.andThen List.head


parseName : Url -> Maybe String
parseName url =
    Url.parse (Url.top <?> Query.string "name") { url | path = "" }
        |> Maybe.join


parseSize : Url -> Maybe Int
parseSize url =
    Url.parse (Url.top <?> Query.int "size") { url | path = "" }
        |> Maybe.join


parseDebug : Url -> Bool
parseDebug url =
    Url.parse (Url.top <?> Query.string "debug") { url | path = "" }
        |> Maybe.join
        |> Maybe.unwrap False (always True)


colors : Data -> Dict String Color
colors data =
    List.zip (Data.categories data) Scale.category10 |> Dict.fromList


toMonth : String -> String
toMonth str =
    let
        month =
            Array.fromList
                [ "Jan"
                , "Feb"
                , "Mar"
                , "Apr"
                , "May"
                , "Jun"
                , "Jul"
                , "Aug"
                , "Sep"
                , "Oct"
                , "Nov"
                , "Dec"
                ]
    in
    String.toInt str
        |> Maybe.andThen (\n -> Array.get (n - 1) month)
        |> Maybe.withDefault "NoN"
