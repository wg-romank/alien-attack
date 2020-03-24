module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, div, text)
import Html.Attributes exposing (width, height, style)
import Html.Events.Extra.Touch as Touch
import Math.Vector2 as Vec2 exposing (vec2, Vec2)
import Dict exposing (Dict)
import Math.Vector4 exposing (vec4)
import WebGL
import WebGL.Texture as Texture exposing (Error, Texture)
import Time
import Task
import Maybe.Extra as ME

import Graphics exposing (drawRectangle, Rectangle, RectDisplay(..), GameObjectType(..), gameObjectTypeToInt)
import GameState exposing (..)

type alias Atlas = Dict Int Texture


playerSprite: Atlas -> GameState -> List Rectangle
playerSprite atlas state =
    let
        userTexture = ME.toList (Dict.get (gameObjectTypeToInt User) atlas)
    in
    List.map
        (\texture -> {
            pos = state.playerPosition.pos,
            width = state.playerPosition.width,
            height = state.playerPosition.height,
            display = RectTexture texture
        }) userTexture

enemySprite: Atlas -> GameState -> List Rectangle
enemySprite atlas state =
    let
        enemyTexture = ME.toList (Dict.get (gameObjectTypeToInt Enemy) atlas)  
    in
        List.concatMap
            (\enemy -> List.map (\texture -> {
                pos = enemy.pos,
                width = enemy.width,
                height = enemy.height,
                display = RectTexture texture
            }) enemyTexture) state.enemies
    

bulletSprite: GameState -> List Rectangle
bulletSprite state =
    List.map (\bullet -> {
        pos = bullet.pos,
        width = bullet.width,
        height = bullet.height,
        display = RectColor (vec4 1.0 1.0 1.0 1.0)
    }) state.rounds

type alias Model = {
    message: String,
    width: Int,
    height: Int,
    atlas: Atlas,
    state: GameState,
    objects: List Rectangle }


init: () -> (Model, Cmd TouchEvent)
init _ = ( {
    message = "",
    width = 160,
    height = 240,
    atlas = Dict.empty,
    state = initialState,
    objects = [ ] }, 
    loadAtlas )

loadAtlas: Cmd TouchEvent
loadAtlas =
        Task.attempt AtlasLoaded (
            List.map
            (
                \(typ, url) ->
                    Texture.load url 
                    |> Task.map (\tex -> (gameObjectTypeToInt typ, tex) )
            )
            [
                (Enemy, "https://wg-romank.github.io/alien-attack/assets/Octo-1.png"),
                (User, "https://wg-romank.github.io/alien-attack/assets/Player_v1-1.png")
            ] |> Task.sequence
              |> Task.map Dict.fromList
        )
    

type TouchEvent
    = Start (Float, Float)
    | Move (Float, Float)
    | End (Float, Float)
    | Delta Float
    | AtlasLoaded (Result Error Atlas)

touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )

view: Model -> Html TouchEvent
view model =
        div []
        [
            WebGL.toHtml
            [
              Touch.onStart (Start << touchCoordinates),
              Touch.onMove (Move << touchCoordinates),
              Touch.onEnd (End << touchCoordinates),
              width model.width,
              height model.height,
              style "height" "100vh",
              style "image-rendering" "webkit-optimize-contrast",
              style "backgroundColor" "#000000",
              style "display" "block" ]
            (List.map (\o -> drawRectangle o (vec2 (toFloat model.width) (toFloat model.height) )) model.objects)
            -- text <| Debug.toString model
        ]

objectsToDraw: Atlas -> GameState -> List Rectangle
objectsToDraw atlas state = List.concat
    [
        playerSprite atlas state,
        enemySprite atlas state
    ]


update: TouchEvent -> Model -> (Model, Cmd TouchEvent)
update event model =
    case event of
        -- Start (x, y) -> ({ model | from = vec2 x y }, Cmd.none)
        Move (x, y) ->
            ({ model | state = registerUserInput (PlayerMove(x, y)) model.state }, Cmd.none)
        End (x, y) ->
            ({ model | state = registerUserInput PlayerFire model.state }, Cmd.none)
        Delta delta -> 
            let newState = step delta model.state
            in
                ({ model | state = newState, objects = objectsToDraw model.atlas newState }, Cmd.none)
        AtlasLoaded result ->
            let
                state = model.state
            in case result of
                Result.Ok atlas ->
                    ({model | message = "atlas loaded", atlas = atlas, objects = objectsToDraw atlas state}, Cmd.none)
                Result.Err err ->
                    ({model | message = Debug.toString err}, Cmd.none)
        _ -> (model, Cmd.none)

main = Browser.element {
          init = init,
          subscriptions = \_ -> Delta |> onAnimationFrameDelta,
          view = view,
          update = update
       }
