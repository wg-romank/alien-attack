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
        width = 16.0,
        height = 16.0,
        display = RectTexture texture
    }

enemy: Texture -> Rectangle
enemy texture = {
        typ = Enemy,
        pos = vec2 56 24,
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

movePlayer: Rectangle -> Model -> Vec2 -> Rectangle
movePlayer obj model to =
        Vec2.sub to model.from
        |> Vec2.add obj.pos
        |> \p -> vec2
                    ( Basics.clamp 0.0 (toFloat model.width - obj.width) (Vec2.getX p) )
                    ( Basics.clamp ( 2.0 * (toFloat model.height - obj.height) / 3.0 ) (toFloat model.height - obj.height) (Vec2.getY p) )
        |> \q -> { obj | pos = q }


modFloat: Float -> Float -> Float -> Float
modFloat n from to =
    if n > to then from + (n - to)
    else if n < from then to - n
        else n

modCoordinates: Float -> Float -> Vec2 -> Vec2
modCoordinates w h v =
    vec2 
        (modFloat (Vec2.getX v) 0 w)
        (modFloat (Vec2.getY v) 0 h)

moveEnemy: Rectangle -> Model -> Float -> Rectangle
moveEnemy obj model time =
        Vec2.add obj.pos (vec2 0.0 (time / 200.0) )
        |> \p -> vec2 (Vec2.getX p + (sin time / 200.0) * (toFloat model.width / 2.0) ) (Vec2.getY p) 
        |> modCoordinates (toFloat model.width - obj.width) (toFloat model.height - obj.height)
        |> \q -> { obj | pos = q }



initialObjects: Atlas -> List Rectangle
initialObjects atlas = List.concat
        [List.map user (ME.toList (Dict.get (gameObjectTypeToInt User) atlas)) ,
         List.map enemy (ME.toList (Dict.get (gameObjectTypeToInt Enemy) atlas)) ]

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
                        User -> movePlayer obj model (vec2 x y)
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
                model.objects,
            t = delta
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
