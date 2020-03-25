module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div)
import Html.Attributes exposing (width, height, style)
import Task

import Html.Events.Extra.Touch as Touch
import WebGL
import WebGL.Texture exposing (Error)

import Graphics exposing (RectDisplay(..))
import GameState exposing (..)
import Sprites exposing (..)

type alias Model = {
    message: String,
    atlas: Atlas,
    state: GameState }


init: () -> (Model, Cmd Event)
init _ = ( {
    message = "",
    atlas = emptyAtlas,
    state = initialState }, Task.attempt AtlasLoaded loadAtlas )
    

type Event
    = Start (Float, Float)
    | Move (Float, Float)
    | End (Float, Float)
    | Delta Float
    | AtlasLoaded (Result Error Atlas)

touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )

view: Model -> Html Event
view model =
        div []
        [
            WebGL.toHtml
            [
              Touch.onStart (Start << touchCoordinates),
              Touch.onMove (Move << touchCoordinates),
              Touch.onEnd (End << touchCoordinates),
              width model.state.boardSize.width,
              height model.state.boardSize.height,
            --   style "height" "100vh",
              style "image-rendering" "webkit-optimize-contrast",
              style "backgroundColor" "#000000",
              style "display" "block" ]
              (objectsToDraw model.atlas model.state)
            -- text <| Debug.toString model
        ]


update: Event -> Model -> (Model, Cmd Event)
update event model =
    case event of
        -- Start (x, y) -> ({ model | from = vec2 x y }, Cmd.none)
        Move (x, y) ->
            ({ model | state = registerUserInput (PlayerMove(x, y)) model.state }, Cmd.none)
        End (x, y) ->
            ({ model | state = registerUserInput PlayerFire model.state }, Cmd.none)
        Delta delta -> 
            ({ model | state = step delta model.state }, Cmd.none)
        AtlasLoaded result ->
            case result of
                Result.Ok atlas -> ({model | atlas = atlas}, Cmd.none)
                Result.Err _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)

main: Program() Model Event
main = Browser.element {
       init = init,
       subscriptions = \_ -> Delta |> onAnimationFrameDelta,
       view = view,
       update = update
    }
