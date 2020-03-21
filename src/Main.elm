module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events.Extra.Touch as Touch
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import WebGL

import Graphics exposing (drawRectangle)

type alias Model = {
    width: Int,
    height: Int,
    pos: Vec2,
    from: Vec2 }

init: Model
init = { width = 160, height = 240, pos = vec2 0.0 0.0, from = vec2 0.0 0.0 }

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
            { pos = model.pos, width = 16.0, height = 16.0 }
            (vec2 (toFloat model.width) (toFloat model.height) ) ]

moveTo: Model -> Vec2 -> Vec2
moveTo model to =
        Vec2.sub to model.from
        |> Vec2.add model.pos
        |> \p -> vec2
                    ( Basics.clamp 0.0 (toFloat model.width - 16) (Vec2.getX p) )
                    ( Basics.clamp 0.0 (toFloat model.height - 16) (Vec2.getY p) )


update: TouchEvent -> Model -> Model
update event model =
    case event of
        Start (x, y) -> {
            width = model.width,
            height = model.height,
            pos = model.pos,
            from = vec2 x y }
        Move (x, y) -> {
            width = model.width,
            height = model.height,
            pos = moveTo model (vec2 x y) ,
            from = vec2 x y }
        _ -> model

main = Browser.sandbox { init = init, view = view, update = update }