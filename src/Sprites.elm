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

type GameObjectType = User1 | User2 | User3 | Enemy1 | Enemy2 | Bullet

gameObjectTypeToInt: GameObjectType -> Int
gameObjectTypeToInt typ =
    case typ of
       User1 -> 1
       User2 -> 2
       User3 -> 3
       Enemy1 -> 4
       Enemy2 -> 5
       Bullet -> 6

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
            Texture.load url 
            |> Task.map (\tex -> (gameObjectTypeToInt typ, tex) )
    )
    [
        (Enemy1, "https://wg-romank.github.io/alien-attack/assets/Octo-1.png"),
        (Enemy2, "https://wg-romank.github.io/alien-attack/assets/Octo-2.png"),
        (User1, "https://wg-romank.github.io/alien-attack/assets/Player_v1-1.png"),
        (User2, "https://wg-romank.github.io/alien-attack/assets/Player_v1-2.png"),
        (User3, "https://wg-romank.github.io/alien-attack/assets/Player_v1-3.png")
    ] |> Task.sequence
      |> Task.map Dict.fromList

objectsToDraw: Atlas -> GameState -> List WebGL.Entity
objectsToDraw atlas state = List.concat
    [
        playerSprite atlas state,
        enemySprite atlas state,
        bulletSprite state
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
            display = RectTexture texture
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
                       Just t -> [{ pos = roundPos enemy.pos, width = enemy.width, height = enemy.height, display = RectTexture t }]
                       Nothing -> []
            ) state.enemies
    

bulletSprite: GameState -> List Rectangle
bulletSprite state =
    List.map (\bullet -> {
        pos = roundPos bullet.pos,
        width = bullet.width,
        height = bullet.height,
        display = RectColor (vec4 1.0 1.0 1.0 1.0)
    }) state.rounds