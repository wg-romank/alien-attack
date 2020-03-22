module Graphics exposing (drawRectangle, Rectangle, RectDisplay(..))

import Math.Vector4 as Vec4 exposing (vec4, Vec4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture as Texture exposing (Error, Texture)
type alias Vertex = { position : Vec2 }
type alias Uniforms = { u_res: Vec2, vcolor: Vec4, texture: Texture }

vertexShader = 
    [glsl|
        attribute vec2 position;
        uniform vec2 u_res;
        varying vec2 vcoord;

        void main() {
            vec2 zeroOne = position / u_res;
            vec2 clipSpace = zeroOne * 2.0 - 1.0;
            gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
            vcoord = clipSpace * vec2(1, -1);
        }
    |]

fragmentColorShader =
    [glsl|
        precision mediump float;
        uniform vec4 vcolor;
        varying vec2 vcoord;
        void main() {
            gl_FragColor = vcolor;
        }
    |]

fragmentTextureShader =
    [glsl|
        precision mediump float;
        uniform sampler2d texture;
        varying vec2 vcoord;
        void main() {
            gl_FragColor = texture2D(texture, vcoord);
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
    display: RectDisplay }

type RectDisplay =
    RectColor Vec4 |
    RectTexture Texture

drawRectangle: Rectangle -> Vec2 -> WebGL.Entity
drawRectangle rec res = 
            WebGL.entity
                vertexShader
                fragmentColorShader
                (rect rec.pos rec.width rec.height)
                { u_res = res, vcolor = vec4 0.5 0.0 1.0 1.0 } 

            -- case rec.display of
            --     RectColor c ->
            --         WebGL.entity
            --             vertexShader
            --             fragmentColorShader
            --             (rect rec.pos rec.width rec.height)
            --             { u_res = res, vcolor = c } 
            --     RectTexture t ->
            --         WebGL.entity
            --             vertexShader
            --             fragmentTextureShader
            --             (rect rec.pos rec.width rec.height)
            --             { u_res = res, texture = t } 

