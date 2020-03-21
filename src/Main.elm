module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events.Extra.Touch as Touch
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import WebGL

import Graphics exposing (drawRectangle, Rectangle)

type alias Model = {
    width: Int,
    height: Int,
    from: Vec2,
    player: Rectangle }

init: Model
init = { 
    width = 160,
    height = 240,
    from = vec2 0.0 0.0,
    player = {
        pos = vec2 0 0,
        width = 16.0,
        height = 16.0,
        color = vec4 0.5 0 1 1
    } }

type TouchEvent
    = Start (Float, Float)
    | Move (Float, Float)
    | End (Float, Float)

type Msg = TouchEvent

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

moveTo: Rectangle -> Vec2 -> Vec2 -> Int -> Int -> Rectangle
moveTo player from to width height =
        Vec2.sub to from
        |> Vec2.add player.pos
        |> \p -> vec2
                    ( Basics.clamp 0.0 (toFloat width - 16) (Vec2.getX p) )
                    ( Basics.clamp 0.0 (toFloat height - 16) (Vec2.getY p) )
        |> \q -> { player | pos = q }


update: TouchEvent -> Model -> Model
update event model =
    case event of
        Start (x, y) -> {
            model |
            player = model.player,
            from = vec2 x y }
        Move (x, y) -> {
            model |
            player = moveTo model.player model.from (vec2 x y) model.width model.height,
            from = vec2 x y }
        _ -> model

main = Browser.sandbox { init = init, view = view, update = update }