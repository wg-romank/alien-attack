module Graphics exposing (drawRectangle, Rectangle)

import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture)
type alias Vertex = { position : Vec2 }
vertexShader : Shader Vertex { u_res: Vec2, vcolor: Vec4 } { }
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

fragmentShader : Shader { } { u_res: Vec2, vcolor: Vec4 } { }
fragmentShader =
    [glsl|
        precision mediump float;
        uniform vec4 vcolor;
        void main() {
            gl_FragColor = vcolor;
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
    height: Float,
    color: Vec4,
    texture: Maybe Texture }

drawRectangle: Rectangle -> Vec2 -> WebGL.Entity
drawRectangle rec res = WebGL.entity
                vertexShader
                fragmentShader
                (rect rec.pos rec.width rec.height)
                { u_res = res, vcolor = rec.color } 

