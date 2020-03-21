module Main exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Html.Events.Extra.Touch as Touch
import Math.Vector2 exposing (vec2, Vec2)
import Math.Vector3 exposing (vec3, Vec3)
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

-- x : Int -> Mesh Vertex
-- x pos = WebGL.points [ List.map (\p -> Vertex (vec2 (toFloat p) (toFloat p) ) ) (List.range 1 pos) ]


mesh : Mesh Vertex
mesh =
    WebGL.triangles
        [ ( Vertex (vec2 0 0)
          , Vertex (vec2 1 1)
          , Vertex (vec2 1 -1)
          ) ]

makeEntity: Int -> WebGL.Entity
makeEntity pos = WebGL.entity
                vertexShader
                fragmentShader
                mesh
                -- (x pos) { 
                { } 
                -- { u_resolution = vec2 640 480 } 

view: Model -> Html msg
view pos = WebGL.toHtml 
        [ width 640, height 480, style "backgroundColor" "#9ea7b8", style "display" "block" ]
        [ makeEntity pos ]

vertexShader : Shader { position: Vec2 } { } { }
vertexShader = 
    [glsl|
        attribute vec2 position;

        void main() {
            gl_Position = vec4(position, 0, 1);
        }
    |]
            -- vec2 zeroOne = position / resolution;
            -- vec2 zeroTwo = position * 2.0;
            -- vec2 clipSpace = zeroTwo - 1.0;
            -- gl_Position = vec4(clipSpace * vec(1, -1), 0, 1);

fragmentShader : Shader {} { } { }
fragmentShader =
    [glsl|
        void main() {
            gl_FragColor = vec4(1.0, 0.0, 1.0, 1.0);
        }
    |]

main = Browser.sandbox { init = init, view = view, update = always }