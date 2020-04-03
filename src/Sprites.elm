module Sprites exposing (objectsToDraw)

import WebGL
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector4 exposing (Vec4, vec4)
import Maybe.Extra as ME

import Atlas exposing (..)
import GameState exposing (GameState, widthFloat, heightFloat)
import Shaders exposing (..)

depthLayerProjectiles: DepthLayer
depthLayerProjectiles = { near = 0.0, far = 0.1 }

depthLayerCharacters: DepthLayer
depthLayerCharacters = { near = 0.1, far = 0.2 }

roundPos: Vec2 -> Vec2
roundPos pos =
    vec2
        (Vec2.getX pos |> round |> toFloat)
        (Vec2.getY pos |> round |> toFloat)


objectsToDraw: Atlas -> GameState -> List WebGL.Entity
objectsToDraw atlas state = List.concat
    [
        backgroundSprite atlas state,
        playerSprite atlas state,
        enemySprite atlas state,
        bulletSprite state,
        enemyBulletSprite state
    ] |> List.map (\rect ->
        drawRectangle (
            vec2
                (state.boardSize |> widthFloat)
                (state.boardSize |> heightFloat))
            rect)

playerSprite: Atlas -> GameState -> List Rectangle
playerSprite atlas state =
    let
        userTexture = ME.toList (Atlas.get atlas User1)
    in
    List.map
        (\texture -> {
            pos = roundPos state.playerPosition.pos,
            width = state.playerPosition.width,
            height = state.playerPosition.height,
            display = RectTexture texture,
            depthLayer = depthLayerCharacters
        }) userTexture

enemySprite: Atlas -> GameState -> List Rectangle
enemySprite atlas state =
    let
        frameSwitch = 2
    in
    List.concatMap
        (\enemy ->
            let
                textureKey = if (enemy.sinceSpawned / 1000.0 |> round |> modBy frameSwitch) == 0 then Enemy1 else Enemy2
                maybeTexture = Atlas.get atlas textureKey
            in
                case maybeTexture of
                   Just t -> [{
                       pos = roundPos enemy.pos,
                       width = enemy.width,
                       height = enemy.height,
                       display = RectTexture t,
                       depthLayer = depthLayerCharacters }]
                   Nothing -> []
        ) state.enemies
    

bulletSprite: GameState -> List Rectangle
bulletSprite state =
    List.map (\bullet -> {
        pos = roundPos bullet.pos,
        width = bullet.width,
        height = bullet.height,
        display = RectColor (vec4 1.0 1.0 1.0 1.0),
        depthLayer = depthLayerProjectiles
    }) state.rounds


enemyBulletSprite: GameState -> List Rectangle
enemyBulletSprite state =
    List.map (\bullet -> {
        pos = roundPos bullet.pos,
        width = bullet.width,
        height = bullet.height,
        display = RectColor (vec4 1.0 0.5 1.0 1.0),
        depthLayer = depthLayerProjectiles
    }) state.enemyRounds

rgbColor: Float -> Float -> Float -> Vec4
rgbColor r g b = vec4 (r / 255.0) (g / 255.0) (b / 255.0) 1.0


backgroundSprite: Atlas -> GameState -> List Rectangle
backgroundSprite atlas state =
    let
        w = widthFloat state.boardSize
        h = heightFloat state.boardSize
        yScroll = h - (3 * state.bgOffset)
        pos = vec2 0 yScroll
    in
        List.concat
        [
            [{ pos = vec2 0 0, width = w, height = h, display = RectColor (rgbColor 40.0 53.0 31.0), depthLayer = { near = 0.9, far = 1.0 } }],
            List.map (\t -> { pos = roundPos (vec2 0.0 0.0), width = w, height = h, display = RectTexture t, depthLayer = { near = 0.8, far = 0.9 } })
            (Atlas.get atlas BackgroundStars |> ME.toList),
            List.map (\t -> { pos = roundPos pos, width = w, height = 69.0, display = RectTexture t , depthLayer = { near = 0.7, far = 0.8 } })
            (Atlas.get atlas BackgroundPlanet |> ME.toList)
        ]
