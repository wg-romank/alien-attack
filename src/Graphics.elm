module Graphics exposing (drawRectangle, Rectangle, RectDisplay(..))

import Math.Vector4 exposing (Vec4)
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import WebGL exposing (Mesh)
import WebGL.Texture exposing (Texture)
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest

type alias Vertex = {
        position : Vec2,
        textureCoord: Vec2
    }

vertexShader = 
    [glsl|
        attribute vec2 position;
        attribute vec2 textureCoord;
        uniform vec2 u_res;
        varying vec2 vtextureCoord;

        void main() {
            vec2 zeroOne = position / u_res;
            vec2 clipSpace = zeroOne * 2.0 - 1.0;
            gl_Position = vec4(clipSpace * vec2(1, -1), 0, 1);
            vtextureCoord = textureCoord;
        }
    |]

fragmentColorShader =
    [glsl|
        precision mediump float;
        uniform vec4 vcolor;
        varying vec2 vtextureCoord;
        void main() {
            gl_FragColor = vcolor;
        }
    |]

fragmentTextureShader =
    [glsl|
        precision mediump float;
        uniform sampler2D texture;
        varying vec2 vtextureCoord;
        void main() {
            gl_FragColor = texture2D(texture, vtextureCoord);
        }
    |]


rect : Vec2 -> Float -> Float -> Mesh Vertex
rect point width height = 
    WebGL.indexedTriangles
        [ 
          Vertex point (vec2 1 1),
          Vertex ( Vec2.add point (vec2 width 0.0) ) (vec2 0 1),
          Vertex ( Vec2.add point (vec2 0.0 height) ) (vec2 1 0),
          Vertex ( Vec2.add point (vec2 width height) ) (vec2 0 0)
        ]
        [ (0, 1, 2), (2, 1, 3) ]


type alias Rectangle = {
    near: Float,
    far: Float,
    pos: Vec2,
    width: Float,
    height: Float,
    display: RectDisplay }

type RectDisplay =
    RectColor Vec4 |
    RectTexture Texture

drawRectangle: Vec2 -> Rectangle -> WebGL.Entity
drawRectangle res rec = 
    case rec.display of
        RectColor c ->
            WebGL.entityWith
                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha,
                  DepthTest.less { write = True, near = rec.near, far = rec.far } ]
                vertexShader
                fragmentColorShader
                (rect rec.pos rec.width rec.height)
                { u_res = res, vcolor = c } 
        RectTexture t ->
            WebGL.entityWith
                -- ensure transparent texture blending
                [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha,
                  DepthTest.less { write = True, near = rec.near, far = rec.far } ]
                vertexShader
                fragmentTextureShader
                (rect rec.pos rec.width rec.height)
                { u_res = res, texture = t } 