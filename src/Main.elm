module Main exposing (main)

import Random
import Browser
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown)
import Browser.Dom exposing (Viewport)
import Html exposing (Html, div)
import Html.Attributes exposing (width, height, style)
import Task

import Html.Events.Extra.Touch as Touch
import WebGL
import WebGL.Texture exposing (Error)

import Json.Decode as Decode

import Graphics exposing (RectDisplay(..))
import GameState exposing (..)
import Sprites exposing (..)

type alias Model = {
    viewportHeight: Int,
    viewportWidth: Int,
    viewportMultiplier: Float,
    message: String,
    atlas: Atlas,
    state: GameState }



init: () -> (Model, Cmd Msg)
init _ = ( {
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

view: Model -> Html Msg
view model =
        div [
            Html.Attributes.align "center"
        ]
        [
            WebGL.toHtml
            [
              Touch.onStart (Start << touchCoordinates),
              Touch.onMove (Move << touchCoordinates),
              Touch.onEnd (End << touchCoordinates),
              width model.state.boardSize.width,
              height model.state.boardSize.height,
              -- style "image-rendering" "-webkit-optimize-contrast",
              style "image-rendering" "crisp-edges",
            --   style "width" (String.fromInt model.viewportWidth ++ "px"),
              style "height" (String.fromInt model.viewportHeight ++ "px"),
              style "backgroundColor" "#000000",
              style "display" "block" ]
              (objectsToDraw model.atlas model.state)
            -- text <| Debug.toString model
        ]


computeViewportSize: Viewport -> Model -> Model
computeViewportSize viewport model =
    let
        vph = viewport.viewport.height
        vpm = viewport.viewport.height / (toFloat model.state.boardSize.height)
        ratio = (toFloat model.state.boardSize.height) / (toFloat model.state.boardSize.width)
        vpw = vph / ratio
    in 
    {model |
        viewportWidth = Basics.round vpw,
        viewportHeight = Basics.round vph,
        viewportMultiplier = vpm }


update: Msg -> Model -> (Model, Cmd Msg)
update event model =
    case event of
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
main = Browser.element {
       init = init,
       subscriptions = \_ -> Sub.batch [ onAnimationFrameDelta Delta,
              onKeyDown keyDecoder],
       view = view,
       update = update
    }
