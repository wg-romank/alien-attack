module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (width, height, style)
import Html.Events.Extra.Touch as Touch
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector4 exposing (vec4, Vec4)
import WebGL exposing (Mesh, Shader)

type alias Model = { width: Int, height: Int, pos: Vec2, from: Vec2 }
init: Model
init = { width = 160, height = 240, pos = (vec2 0.0 0.0), from = (vec2 0.0 0.0)}

type TouchEvent
    = None
    | Start (Float, Float)
    | Move (Float, Float)
    | End (Float, Float)

type Msg = TouchEvent

type alias Vertex = { position : Vec2 }
-- x : Int -> Mesh Vertex
-- x pos = WebGL.points [ List.map (\p -> Vertex (vec2 (toFloat p) (toFloat p) ) ) (List.range 1 pos) ]


rect : Vec2 -> Float -> Float -> Mesh Vertex
rect point width height = 
    WebGL.indexedTriangles
        [ 
          Vertex ( Vec2.add point (vec2 width height) ),
          Vertex ( Vec2.add point (vec2 width 0.0) ),
          Vertex point,
          Vertex ( Vec2.add point (vec2 0.0 height) ) ]
        [ (0, 1, 2), (2, 3, 0) ]

makeEntity: Vec2 -> Vec2 -> WebGL.Entity
makeEntity pos res = WebGL.entity
                vertexShader
                fragmentShader
                (rect pos 16.0 16.0)
                { u_res = res } 

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
        [ makeEntity model.pos (vec2 (toFloat model.width) (toFloat model.height) ) ]

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

vertexShader : Shader Vertex { u_res: Vec2 } {}
vertexShader = 
    [glsl|
        attribute vec2 position;
        uniform vec2 u_res;

        void main() {
            vec2 zeroOne = position / u_res;
            vec2 clipSpace = zeroOne * 2.0 - 1.0;
            gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
        }
    |]

fragmentShader : Shader { } { u_res: Vec2 } {}
fragmentShader =
    [glsl|
        void main() {
            gl_FragColor = vec4(0.5, 0.5, 1.0, 1.0);
        }
    |]

main = Browser.sandbox { init = init, view = view, update = update }