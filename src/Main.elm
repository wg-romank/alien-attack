module Main exposing (main)

import Random
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Browser.Dom exposing (Viewport)
import Html exposing (Html, div, text, p)
import Html.Attributes exposing (width, height, style)
import Task

import Math.Vector2 exposing (Vec2)

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
    atlas: Atlas,
    state: GameState
  }


init: () -> (Model, Cmd Msg)
init _ = ( {
    paused = False,
    offset = 0,
    viewportWidth = 0,
    viewportHeight = 0,
    viewportMultiplier = 1,
    atlas = emptyAtlas,
    state = initialState },
        Cmd.batch [
            Task.attempt AtlasLoaded loadAtlas,
            Task.perform ViewPortLoaded Browser.Dom.getViewport
        ]
    )
    
type Msg
    =
    Start (Float, Float)
    | Move (Float, Float)
    | TouchEnd (Float, Float)
    | Delta Float
    | AtlasLoaded (Result Error Atlas)
    | ViewPortLoaded (Viewport)
    | Pause
    | Resume
    | KeyboardLeft
    | KeyboardRight
    | KeyboardFire
    | EnemiesRoll (List EnemyAction)
    | EnemiesSpawnRoll (List Vec2)
    | Other (String)

type alias Document msg = {
  title : String,
  body : List (Html msg) }

view: Model -> Document Msg
view model =
  {
    title = "Main",
    body = [
      if model.atlas |> loaded |> not then messageScreen "LOADING..." model
      else if model.state.playerDead then messageScreen ("YOUR SCORE: " ++ String.fromInt model.state.score) model
      else if model.state.playerDeorbited then messageScreen "DEORBITED" model
      else simulationScreen model
    ]
  }

container: Model -> List (Html.Attribute html) -> List (Html html) -> Html html
container model attrs msgs =
  div (attrs ++ [
      style "position" "absolute",
      style "top" "0",
      style "left" (String.fromInt model.offset ++ "px"),
      style "height" (String.fromInt model.viewportHeight ++ "px"),
      style "width" (String.fromInt model.viewportWidth ++ "px")
  ]) msgs

messageScreen: String -> Model -> Html Msg
messageScreen message model =
  container model [ style "backgroundColor" "#000000" ] [
    p
      [
        style "color" "#FFFFFF",
        style "top" "50%",
        style "position" "absolute",
        style "text-align" "center",
        style "width" (String.fromInt model.viewportWidth ++ "px")
      ]
      [ text message ]
  ]


hudAnnouncement: String -> Model -> Html msg
hudAnnouncement t model =
  p [
    style "color" "#FFFFFF",
    style "position" "absolute",
    style "text-align" "center",
    style "width" (String.fromInt model.viewportWidth ++ "px")
  ] [ text t ]

hudText: String -> String -> String -> String -> String -> Html msg
hudText margin1 margin1Amount margin2 margin2Amount t =
  p
    [
      style "position" "absolute",
      style "color" "#FFFFFF",
      style margin1 margin1Amount,
      style margin2 margin2Amount
    ]
    [ text t ]


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )

simulationScreen: Model -> Html Msg
simulationScreen model =
        div [
            Html.Attributes.align "center",
            Touch.onStart (Start << touchCoordinates),
            Touch.onMove (Move << touchCoordinates),
            Touch.onEnd (TouchEnd << touchCoordinates)
        ]
        [
          WebGL.toHtmlWith [ WebGL.alpha True, WebGL.depth 1 ]
          [
            width model.state.boardSize.width,
            height model.state.boardSize.height,
            style "position" "absolute",
            style "top" "0",
            style "left" (String.fromInt model.offset ++ "px"),
            style "height" (String.fromInt model.viewportHeight ++ "px"),
            style "display" "block" ]
            (objectsToDraw model.atlas model.state),
          container model [] [
            hudText "bottom" "2%" "left" "3%" ("FUEL: " ++ String.fromFloat model.state.fuel),
            hudText "bottom" "6%" "left" "3%" ("COURSE: " ++ String.fromFloat model.state.course),
            if model.state.wave == wavesMax then
              hudText "top" "1%" "left" "3%" ("SCORE: " ++ String.fromInt model.state.score )
            else
              hudAnnouncement ("WAVE " ++ String.fromInt model.state.wave) model ]
        ]


computeViewportSize: Viewport -> Model -> Model
computeViewportSize viewport model =
    let
        vph = viewport.viewport.height
        vpm = viewport.viewport.height / toFloat model.state.boardSize.height
        ratio = toFloat model.state.boardSize.height / toFloat model.state.boardSize.width
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
        TouchEnd (x, y) ->
          let
            vm = model.viewportMultiplier
          in
            ({ model | state = registerUserTap ((x - toFloat model.offset) /vm , y / vm) model.state }, Cmd.none)

        KeyboardLeft -> ({ model | state = registerUserInput PlayerMoveLeft model.state }, Cmd.none)
        KeyboardRight -> ({ model | state = registerUserInput PlayerMoveRight model.state }, Cmd.none)
        KeyboardFire -> ({model | state = registerUserInput PlayerFire model.state }, Cmd.none)

        EnemiesRoll rolls ->
          let
            state = model.state
            newState = { state | enemyRoll = rolls }
          in
            ({ model | state = newState }, Cmd.none)

        EnemiesSpawnRoll spawnRoll ->
          let
            state = model.state
            newState = { state | enemySpawnRoll = spawnRoll }
          in
            ({model | state = newState}, Cmd.none)

        AtlasLoaded result ->
            case result of
                Result.Ok atlas -> ({model | atlas = atlas}, Cmd.none)
                Result.Err _ -> (model, Cmd.none)

        ViewPortLoaded viewport -> (computeViewportSize viewport model, Cmd.none)

        Delta delta -> 
            let
              newState = gameLoop delta model.state
            in
              ({ model | state = newState, paused = isOver newState },
                Cmd.batch [
                  Random.generate EnemiesRoll (enemiesRoll newState),
                  Random.generate EnemiesSpawnRoll (enemySpawnRoll newState)
                ] )
        _ -> (model, Cmd.none)

toDirection : String -> Msg
toDirection string =
  case string of
    "ArrowLeft" -> KeyboardLeft
    "4" -> KeyboardLeft
    "6" -> KeyboardRight
    "ArrowRight" -> KeyboardRight
    " " -> KeyboardFire
    "1" -> KeyboardFire
    "5" -> KeyboardFire
    other -> Other other

keyDecoder : Decode.Decoder Msg
keyDecoder =
  Decode.map toDirection (Decode.field "key" Decode.string)


main: Program() Model Msg
main = Browser.document {
       init = init,
       subscriptions = \model ->
        Sub.batch [
          if model.paused || Atlas.loaded model.atlas |> not then Sub.none else onAnimationFrameDelta Delta,
          onKeyDown keyDecoder,
          Browser.Events.onVisibilityChange (\v ->
            case v of
              Browser.Events.Hidden -> Pause
              Browser.Events.Visible -> Resume)
          ],
       view = view,
       update = update
    }
