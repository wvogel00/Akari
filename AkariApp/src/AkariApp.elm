module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import File exposing (File)
import Json.Decode as D

main = Browser.element
    { init = init
    , update=update
    , view=view
    , subscription = subscription}

type alias Model = {tweet : String, count : Int}

init : Model
init = {tweet = "", hover = False}

type Msg = Increment | Decrement | Change String | Tweet

update : Msg -> Model -> Model
update msg model = case msg of
    Increment -> model -- {model | count = count + 2}
    Decrement -> model -- {model | count = count - 1}
    Change str -> {model | tweet = str}
    Tweet -> {model | tweet = ""}

view : Model -> Html Msg
view model =
    div []
    [ input [placeholder "ツイート内容を入力", value model.tweet, onInput Change][]
    , button [onClick Tweet] [text "送信"]
    , button [onClick Decrement] [text "-減る"]
    --, div [] [text (String.fromInt model)]
    , button [onClick Increment] [text "増える"]
    ]