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

type alias Atlas = Dict Int Texture

user: Texture -> Rectangle
user texture = {
        typ = User,
        pos = vec2 72 224,
        from = vec2 72 224,
        width = 16.0,
        height = 16.0,
        display = RectTexture texture
    }

enemy: Texture -> Rectangle
enemy texture = {
        typ = Enemy,
        pos = vec2 72 24,
        from = vec2 72 24,
        width = 32.0,
        height = 32.0,
        display = RectTexture texture
    }

type alias Model = {
    counter: Int,
    message: String,
    t: Float,
    width: Int,
    height: Int,
    from: Vec2,
    atlas: Atlas,
    objects: List Rectangle }

init: () -> (Model, Cmd TouchEvent)
init _ = ( {
    counter = 0,
    message = "",
    t = 0,
    width = 160,
    height = 240,
    from = vec2 72 224,
    atlas = Dict.empty,
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
                (Enemy, "http://192.168.0.107:8888/assets/Octo.png"),
                (User, "http://192.168.0.107:8888/assets/Player_v1.png")
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
              style "backgroundColor" "#000000",
              style "display" "block" ]
            (List.map (\o -> drawRectangle o (vec2 (toFloat model.width) (toFloat model.height) )) model.objects),
            text <| Debug.toString model
        ]

moveTo: Rectangle -> Model -> Vec2 -> Rectangle
moveTo obj model to =
        Vec2.sub to model.from
        |> Vec2.add obj.pos
        |> \p -> vec2
                    ( Basics.clamp 0.0 (toFloat model.width - obj.width) (Vec2.getX p) )
                    ( Basics.clamp 0.0 (toFloat model.height - obj.height) (Vec2.getY p) )
        |> \q -> { obj | pos = q, from = obj.pos }

moveEnemy: Rectangle -> Model -> Float -> Rectangle
moveEnemy obj model time = obj



initialObjects: Atlas -> List Rectangle
initialObjects atlas = List.concat
        [List.map user (ME.toList (Dict.get (gameObjectTypeToInt User) atlas)) ,
         List.map enemy (ME.toList (Dict.get (gameObjectTypeToInt Enemy) atlas)) ]
        -- MaybeExtra.toList (Dict.get atlas (gameObjectTypeToInt User))

update: TouchEvent -> Model -> (Model, Cmd TouchEvent)
update event model =
    case event of
        Start (x, y) -> ({
            model |
            from = vec2 x y }, Cmd.none)
        Move (x, y) -> ({
            model |
            objects = List.map
                (\obj ->
                    case obj.typ of 
                        User -> moveTo obj model (vec2 x y)
                        _ -> obj )
                model.objects,
            from = vec2 x y }, Cmd.none)
        Delta delta -> ({
            model |
            objects = List.map 
                (\obj ->
                    case obj.typ of
                        Enemy -> moveEnemy obj model delta
                        _ -> obj)
                model.objects
            }, Cmd.none)
        AtlasLoaded result ->
            case result of
                Result.Ok t -> ({model | message = "atlas loaded", atlas = t, objects = initialObjects t}, Cmd.none)
                Result.Err t -> ({model | message = Debug.toString t}, Cmd.none)
        _ -> (model, Cmd.none)

main = Browser.element {
          init = init,
          subscriptions = \_ -> Delta |> onAnimationFrameDelta,
          view = view,
          update = update
       }
