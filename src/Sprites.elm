module Sprites exposing (objectsToDraw, Atlas, emptyAtlas, loadAtlas)

import Dict exposing (Dict)
import Task

import WebGL
import WebGL.Texture as Texture exposing (Texture, Error)
import Math.Vector2 as Vec2 exposing (Vec2, vec2)
import Math.Vector4 exposing (vec4)
import Maybe.Extra as ME

import GameState exposing (GameState, widthFloat, heightFloat)
import Graphics exposing (Rectangle, RectDisplay(..), drawRectangle)

type GameObjectType = User1 | User2 | User3 | Enemy1 | Enemy2 | Bullet | BackgroundPlanet | BackgroundStars

gameObjectTypeToInt: GameObjectType -> Int
gameObjectTypeToInt typ =
    case typ of
       User1 -> 1
       User2 -> 2
       User3 -> 3
       Enemy1 -> 4
       Enemy2 -> 5
       Bullet -> 6
       BackgroundPlanet -> 7
       BackgroundStars -> 8

roundPos: Vec2 -> Vec2
roundPos pos =
    vec2
        (Vec2.getX pos |> round |> toFloat)
        (Vec2.getY pos |> round |> toFloat)

type alias Atlas = Dict Int Texture

emptyAtlas: Atlas 
emptyAtlas = Dict.empty

loadAtlas: Task.Task Error Atlas
loadAtlas =
    List.map
    (
        \(typ, url) ->
            Texture.loadWith Texture.nonPowerOfTwoOptions url 
            |> Task.map (\tex -> (gameObjectTypeToInt typ, tex) )
    )
    [
        (Enemy1, "https://wg-romank.github.io/alien-attack/assets/Octo-1.png"),
        (Enemy2, "https://wg-romank.github.io/alien-attack/assets/Octo-2.png"),
        (User1, "https://wg-romank.github.io/alien-attack/assets/Player_v1-1.png"),
        (User2, "https://wg-romank.github.io/alien-attack/assets/Player_v1-2.png"),
        (User3, "https://wg-romank.github.io/alien-attack/assets/Player_v1-3.png"),
        (BackgroundPlanet, "https://wg-romank.github.io/alien-attack/assets/bg_planet.png"),
        (BackgroundStars, "https://wg-romank.github.io/alien-attack/assets/bg_stars.png")
    ] |> Task.sequence
      |> Task.map Dict.fromList

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
        userTexture = ME.toList (Dict.get (gameObjectTypeToInt User1) atlas)
    in
    List.map
        (\texture -> {
            pos = roundPos state.playerPosition.pos,
            width = state.playerPosition.width,
            height = state.playerPosition.height,
            display = RectTexture texture,
            near = 0,
            far = 0.1
        }) userTexture

enemySprite: Atlas -> GameState -> List Rectangle
enemySprite atlas state =
    let
        enemyTexture1 = gameObjectTypeToInt Enemy1
        enemyTexture2 = gameObjectTypeToInt Enemy2
    in
        List.concatMap
            (\enemy ->
                let
                    textureKey = if enemy.frameId == 0 then enemyTexture1 else enemyTexture2
                    maybeTexture = Dict.get textureKey atlas
                in
                    case maybeTexture of
                       Just t -> [{ pos = roundPos enemy.pos, width = enemy.width, height = enemy.height, display = RectTexture t, near = 0, far = 0.1 }]
                       Nothing -> []
            ) state.enemies
    

bulletSprite: GameState -> List Rectangle
bulletSprite state =
    List.map (\bullet -> {
        pos = roundPos bullet.pos,
        width = bullet.width,
        height = bullet.height,
        display = RectColor (vec4 1.0 1.0 1.0 1.0),
        near = 0,
        far = 0.1
    }) state.rounds


enemyBulletSprite: GameState -> List Rectangle
enemyBulletSprite state =
    List.map (\bullet -> {
        pos = roundPos bullet.pos,
        width = bullet.width,
        height = bullet.height,
        display = RectColor (vec4 1.0 0.5 1.0 1.0),
        near = 0,
        far = 0.1
    }) state.enemyRounds


backgroundSprite: Atlas -> GameState -> List Rectangle
backgroundSprite atlas state =
    let
        bgPlanet = gameObjectTypeToInt BackgroundPlanet
        bgStars = gameObjectTypeToInt BackgroundStars
        yScroll = (heightFloat state.boardSize) - (3 * state.bgOffset / 1000.0)
        pos = vec2 0 yScroll
        w = widthFloat state.boardSize
        h = heightFloat state.boardSize
    in
        List.concat
        [
            [{ pos = vec2 0 0, width = w, height = h, display = RectColor (vec4 (40.0 / 255.0) (53.0 / 255.0) (31.0 / 255.0) 1.0), near = 0.9, far = 1.0}],
            List.map (\t -> { pos = roundPos pos, width = w, height = h, display = RectTexture t, near = 0.8, far = 0.9 })
            (Dict.get bgStars atlas |> ME.toList),
            List.map (\t -> { pos = roundPos pos, width = w, height = 69.0, display = RectTexture t , near = 0.7, far = 0.8})
            (Dict.get bgPlanet atlas |> ME.toList),
            [{ pos = vec2 0 (yScroll + 69.0), width = w, height = h, display = RectColor (vec4 (88.0 / 255.0) (140.0 / 255.0) (126.0 / 255.0) 1.0), near = 0.6, far = 0.7}]
        ]
