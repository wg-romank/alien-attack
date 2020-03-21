module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events.Extra.Touch as Touch
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Math.Vector4 exposing (vec4, Vec4)
import WebGL exposing (Mesh, Shader)

type alias Model = Vec2

init: Model
init = vec2 160.0 240.0

type TouchEvent
    = None
    | Start Touch.Event
    | Move Touch.Event
    | End Touch.Event
    | Cancel Touch.Event

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

makeEntity: Vec2 -> WebGL.Entity
makeEntity pos = WebGL.entity
                vertexShader
                fragmentShader
                (rect (vec2 0.0 0.0) 16.0 16.0)
                -- (x pos) { 
                { u_res = pos } 

view: Model -> Html msg
view pos = WebGL.toHtml 
        [ width 160, height 240,
          style "backgroundColor" "#000000",
          style "display" "block" ]
        [ makeEntity pos ]

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

main = Browser.sandbox { init = init, view = view, update = always }