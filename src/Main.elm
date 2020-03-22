module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, text)
import Html.Attributes exposing (width, height, style)
import Html.Events.Extra.Touch as Touch
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector4 exposing (vec4)
import WebGL
import WebGL.Texture as Texture exposing (Error, Texture)
import Time
import Task

import Graphics exposing (drawRectangle, Rectangle, RectDisplay(..))

type alias Model = {
    counter: Int,
    message: String,
    t: Float,
    width: Int,
    height: Int,
    from: Vec2,
    player: Rectangle }

init: () -> (Model, Cmd TouchEvent)
init _ = ( {
    counter = 0,
    message = "",
    t = 0,
    width = 160,
    height = 240,
    from = vec2 0.0 0.0,
    player = {
        pos = vec2 72 224,
        width = 16.0,
        height = 16.0,
        display = RectColor (vec4 0.5 0 1 1)
    } }, Task.attempt TextureLoaded (Texture.load "http://10.81.136.37:8888/assets/Player_v1.png") )

type TouchEvent
    = Start (Float, Float)
    | Move (Float, Float)
    | End (Float, Float)
    | Delta Float
    | TextureLoaded (Result Error Texture)

touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )

view: Model -> Html TouchEvent
view model =
        div []
        [
            WebGL.toHtml
            [
              Touch.onStart (Start << touchCoordinates),
              Touch.onMove (Move << touchCoordinates),
              Touch.onEnd (End << touchCoordinates),
              width model.width,
              height model.height,
              style "backgroundColor" "#000000",
              style "display" "block" ]
            [ drawRectangle
                model.player
                (vec2 (toFloat model.width) (toFloat model.height) ) ],
            text <| Debug.toString model
        ]

moveTo: Rectangle -> Model -> Vec2 -> Rectangle
moveTo player model to =
        Vec2.sub to model.from
        |> Vec2.add player.pos
        |> \p -> vec2
                    ( Basics.clamp 0.0 (toFloat model.width - player.width) (Vec2.getX p) )
                    ( Basics.clamp 0.0 (toFloat model.height - player.height) (Vec2.getY p) )
        |> \q -> { player | pos = q }


update: TouchEvent -> Model -> (Model, Cmd TouchEvent)
update event model =
    case event of
        Start (x, y) -> ({
            model |
            player = model.player,
            from = vec2 x y }, Cmd.none)
        Move (x, y) -> ({
            model |
            player = moveTo model.player model (vec2 x y),
            from = vec2 x y }, Cmd.none)
        -- Delta delta -> ({
        --     model |
        --     player = moveTo model.player model (Vec2.add model.player.pos (vec2 (delta * 1.0 / 1000) (delta * 1.0 / 1000) ) ),
        --     from = model.player.pos }, Cmd.none)
        -- Delta delta -> ({ model | t = model.t + delta}, Cmd.none)
        TextureLoaded result ->
            case result of
                Result.Ok t ->
                    let
                        player = model.player
                        newPlayer = { player | display = RectTexture t }
                    in
                        ({ model | counter = model.counter + 1, player = newPlayer, message = "ok" }, Cmd.none)
                Result.Err t ->
                    ({model | counter = model.counter + 1, message = "err" }, Cmd.none)
        _ -> (model, Cmd.none)

main = Browser.element {
          init = init,
          subscriptions = \_ -> Delta |> onAnimationFrameDelta,
          view = view,
          update = update
       }
