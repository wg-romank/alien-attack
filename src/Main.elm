module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events.Extra.Touch as Touch
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL
import Time

import Graphics exposing (drawRectangle, Rectangle)

type alias Model = {
    t: Float,
    width: Int,
    height: Int,
    from: Vec2,
    player: Rectangle }

init: () -> (Model, Cmd TouchEvent)
init _ = ( { 
    t = 0,
    width = 160,
    height = 240,
    from = vec2 0.0 0.0,
    player = {
        pos = vec2 0 0,
        width = 16.0,
        height = 16.0,
        color = vec4 0.5 0 1 1
    } }, Cmd.none )

type TouchEvent
    = Start (Float, Float)
    | Move (Float, Float)
    | End (Float, Float)
    | Delta Float

touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )

view: Model -> Html TouchEvent
view model =
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
            (vec2 (toFloat model.width) (toFloat model.height) ) ]

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
        Delta delta -> ({
            model |
            player = moveTo model.player model (Vec2.add model.player.pos (vec2 (delta * 1.0 / 1000) (delta * 1.0 / 1000) ) ),
            from = model.player.pos }, Cmd.none)
        _ -> (model, Cmd.none)

main = Browser.element {
          init = init,
          subscriptions = \_ -> Delta |> onAnimationFrameDelta,
          view = view,
          update = update
       }