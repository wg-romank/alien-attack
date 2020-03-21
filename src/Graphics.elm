module Graphics exposing (drawRectangle)

import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import WebGL exposing (Mesh, Shader)
type alias Vertex = { position : Vec2 }
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


rect : Vec2 -> Float -> Float -> Mesh Vertex
rect point width height = 
    WebGL.indexedTriangles
        [ 
          Vertex ( Vec2.add point (vec2 width height) ),
          Vertex ( Vec2.add point (vec2 width 0.0) ),
          Vertex point,
          Vertex ( Vec2.add point (vec2 0.0 height) ) ]
        [ (0, 1, 2), (2, 3, 0) ]

type alias Rectangle = {
    pos: Vec2,
    width: Float,
    height: Float }

drawRectangle: Rectangle -> Vec2 -> WebGL.Entity
drawRectangle rec res = WebGL.entity
                vertexShader
                fragmentShader
                (rect rec.pos rec.width rec.height)
                { u_res = res } 

