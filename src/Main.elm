module Main exposing (main)

import Random
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Browser.Dom exposing (Viewport)
import Html exposing (Html, div, text, p)
import Html.Attributes exposing (width, height, style)
import Task

import Html.Events.Extra.Touch as Touch
import WebGL
import WebGL.Texture exposing (Error)

import Json.Decode as Decode

import GameState exposing (..)
import Sprites exposing (..)
import Atlas exposing (..)

type alias Model = {
    paused: Bool,
    offset: Int,
    viewportHeight: Int,
    viewportWidth: Int,
    viewportMultiplier: Float,
    message: String,
    atlas: Atlas,
    state: GameState }



init: () -> (Model, Cmd Msg)
init _ = ( {
    paused = False,
    offset = 0,
    viewportWidth = 0,
    viewportHeight = 0,
    viewportMultiplier = 1,
    message = "",
    atlas = emptyAtlas,
    state = initialState },
        Cmd.batch [
            Task.attempt AtlasLoaded loadAtlas,
            Task.perform ViewPortLoaded Browser.Dom.getViewport
        ]
    )
    

type Msg
    = Start (Float, Float)
    | Move (Float, Float)
    | End (Float, Float)
    | Delta Float
    | AtlasLoaded (Result Error Atlas)
    | ViewPortLoaded (Viewport)
    | Pause
    | Resume
    | Left
    | Right
    | Fire
    | EnemiesRoll (List EnemyAction)
    | Other (String)

touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )


keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)

toDirection : String -> Msg
toDirection string =
  case string of
    "ArrowLeft" ->
      Left
    "ArrowRight" ->
      Right
    "f" ->
      Fire
    " " ->
      Fire
    other ->
      Other other

type alias Document msg =
    { title : String
    , body : List (Html msg)
    }
view: Model -> Document Msg
view model =
        {
          title = "Main",
        body = [div [
            Html.Attributes.align "center",
            style "position" "relative"
        ]
        [
          WebGL.toHtmlWith [ WebGL.alpha True, WebGL.depth 1 ]
          [
            Touch.onStart (Start << touchCoordinates),
            Touch.onMove (Move << touchCoordinates),
            Touch.onEnd (End << touchCoordinates),
            width model.state.boardSize.width,
            height model.state.boardSize.height,
            -- style "image-rendering" "-webkit-optimize-contrast",
            -- style "position" "absolute",
            style "top" "0",
            style "left" ((String.fromInt model.offset) ++ "px"),
            style "image-rendering" "crisp-edges",
          --   style "width" (String.fromInt model.viewportWidth ++ "px"),
            style "height" (String.fromInt model.viewportHeight ++ "px"),
            -- style "backgroundColor" "#000000",
            -- style "backgroundColor" "#283531",
            style "display" "block" ]
            (objectsToDraw model.atlas model.state),
          -- text <| Debug.toString model
          div [
              style "position" "absolute",
              style "top" "0",
              style "left" (String.fromInt model.offset ++ "px"),
              style "height" (String.fromInt model.viewportHeight ++ "px"),
              style "width" (String.fromInt model.viewportWidth ++ "px")
          ] [
            p
              [
                style "position" "absolute",
                style "color" "#FFFFFF",
                style "font-family" "pixelated",
                style "font-size" "2em",
                style "bottom" "2%",
                style "left" "3%"
              ]
              [ text ("FUEL: " ++ String.fromInt model.state.fuel) ],
            p
              [
                style "position" "absolute",
                style "color" "#FFFFFF",
                style "font-family" "pixelated",
                style "font-size" "2em",
                style "bottom" "6%",
                style "left" "3%"
              ]
              [ text "COURSE" ]
            ]
        ] ]
        }


computeViewportSize: Viewport -> Model -> Model
computeViewportSize viewport model =
    let
        vph = viewport.viewport.height
        vpm = viewport.viewport.height / (toFloat model.state.boardSize.height)
        ratio = (toFloat model.state.boardSize.height) / (toFloat model.state.boardSize.width)
        vpw = vph / ratio
        offset = (viewport.viewport.width - vpw) / 2.0 |> round
    in 
    {model |
        offset = offset,
        viewportWidth = Basics.round vpw,
        viewportHeight = Basics.round vph,
        viewportMultiplier = vpm }


update: Msg -> Model -> (Model, Cmd Msg)
update event model =
    case event of
        Pause -> ({ model | paused = True }, Cmd.none)
        Resume -> ({ model | paused = False}, Cmd.none)
        -- Start (x, y) -> ({ model | from = vec2 x y }, Cmd.none)
        Move (x, y) -> let vm = model.viewportMultiplier in
            ({ model | state = registerUserInput (PlayerMove(x / vm, y / vm)) model.state }, Cmd.none)
        Left -> ({ model | state = registerUserInput PlayerMoveLeft model.state }, Cmd.none)
        Right -> ({ model | state = registerUserInput PlayerMoveRight model.state }, Cmd.none)
        Fire -> ({model | state = registerUserInput PlayerFire model.state }, Cmd.none)
        End (x, y) ->
            ({ model | state = registerUserInput PlayerFire model.state }, Cmd.none)
        Delta delta -> 
            let
              newState = step delta model.state
            in
              ({ model | state = newState }, Random.generate EnemiesRoll (enemiesRoll newState) )
        EnemiesRoll rolls ->
          let
            state = model.state
            newState = { state | enemyRoll = rolls }
          in
            ({ model | state = newState }, Cmd.none)
        AtlasLoaded result ->
            case result of
                Result.Ok atlas -> ({model | atlas = atlas}, Cmd.none)
                Result.Err _ -> (model, Cmd.none)
        ViewPortLoaded viewport -> (computeViewportSize viewport model, Cmd.none)
        -- Other key -> (model, jsonConsole key)
        _ -> (model, Cmd.none)

main: Program() Model Msg
main = Browser.document {
       init = init,
       subscriptions = \model ->
        Sub.batch [
          if model.paused then Sub.none else onAnimationFrameDelta Delta,
          onKeyDown keyDecoder,
          Browser.Events.onVisibilityChange (\v ->
            case v of
              Browser.Events.Hidden -> Pause
              Browser.Events.Visible -> Resume)
          ],
       view = view,
       update = update
    }
