module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (width, height)
import Html.Events.Extra.Touch as Touch
import Math.Vector2 exposing (vec2, Vec2)
import WebGL exposing (Mesh, Shader)

type alias Model = Int

init: Model
init = 150

type TouchEvent
    = None
    | Start Touch.Event
    | Move Touch.Event
    | End Touch.Event
    | Cancel Touch.Event

type Msg = TouchEvent

type alias Vertex =
    { position : Vec2 }

x : Int -> Mesh Vertex
x pos = WebGL.points [ List.map (\p -> Vertex (vec2 (toFloat p) (toFloat p) ) ) (List.range 1 pos) ]

makeEntity: Int -> WebGL.Entity
makeEntity pos = WebGL.entity
                vertexShader
                fragmentShader
                (x pos) { 
                    resolution = vec2 640 480
                } 

view: Model -> Html msg
view pos = WebGL.toHtml 
        [ width 640, height 480 ]
        [ makeEntity pos ]

vertexShader : Shader { position: Vec2 } { resolution: Vec2 } { }
vertexShader = 
    [glsl|
        attribute vec2 position;
        uniform vec2 resolution;

        void main() {
            vec2 zeroOne = position / resolution;
            vec2 zeroTwo = position * 2.0;
            vec2 clipSpace = zeroTwo - 1.0;
            gl_Position = vec4(clipSpace * vec(1, -1), 0, 1);
        }
    |]

fragmentShader : Shader {} { resolution: Vec2 } { }
fragmentShader =
    [glsl|
        uniform vec2 resolution;

        void main() {
            gl_FragColor = vec4(120, 120, 120, 120);
        }
    |]

main = Browser.sandbox { init = init, view = view, update = always }