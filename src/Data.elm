module Data exposing (Data, Msg(..), Output, categories, fetch, update)

import Dict exposing (Dict)
import Http
import Json.Decode as Json
import List.Extra as List
import Url exposing (Url)


type alias Data =
    Dict String (List Output)


type alias Output =
    { day : String
    , media : String
    , url : String
    }


categories : Data -> List String
categories data =
    Dict.values data
        |> List.concat
        |> List.map .media
        |> List.unique


type Msg
    = Fetch Data
    | FetchErr Http.Error


fetch : String -> Cmd Msg
fetch url =
    Http.get
        { url = url
        , expect = Http.expectJson fromResult decoder
        }


update : Msg -> Maybe Data
update msg =
    case msg of
        Fetch data ->
            Just data

        FetchErr err ->
            Debug.log ("fetch error: " ++ Debug.toString err) Nothing


fromResult : Result Http.Error Data -> Msg
fromResult res =
    case res of
        Ok data ->
            Fetch data

        Err err ->
            FetchErr err


decoder : Json.Decoder Data
decoder =
    Json.dict <|
        Json.list <|
            Json.map3 Output
                (Json.field "day" Json.string)
                (Json.field "where" Json.string)
                (Json.field "url" Json.string)
