module Atlas exposing(TextureKey(..), Atlas, get, loadAtlas, emptyAtlas, loaded)

import Dict exposing (Dict)
import Task
import WebGL.Texture as Texture exposing (Texture, Error)

type TextureKey = User1 | User2 | User3 | Enemy1 | Enemy2 | Bullet | BackgroundPlanet | BackgroundStars

gameObjectTypeToInt: TextureKey -> Int
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

type alias Atlas = Dict Int Texture

emptyAtlas: Atlas 
emptyAtlas = Dict.empty

loadAtlas: Task.Task Error Atlas
loadAtlas =
    let
        options = Texture.nonPowerOfTwoOptions
    in
    List.map
    (
        \(typ, url) ->
            Texture.loadWith { options | magnify = Texture.nearest } url 
            |> Task.map (\tex -> (gameObjectTypeToInt typ, tex) )
    )
    [
        (Enemy1, "https://wg-romank.github.io/alien-attack/assets/Octo-1.png"),
        (Enemy2, "https://wg-romank.github.io/alien-attack/assets/Octo-2.png"),
        (User1, "https://wg-romank.github.io/alien-attack/assets/Player_v1-1.png"),
        (User2, "https://wg-romank.github.io/alien-attack/assets/Player_v1-2.png"),
        (User3, "https://wg-romank.github.io/alien-attack/assets/Player_v1-3.png"),
        (BackgroundPlanet, "https://wg-romank.github.io/alien-attack/assets/2x3/bg_planet.png"),
        (BackgroundStars, "https://wg-romank.github.io/alien-attack/assets/2x3/bg_stars.png")
    ] |> Task.sequence
      |> Task.map Dict.fromList

loaded: Atlas -> Bool
loaded atlas = Dict.isEmpty atlas |> not

get: Atlas -> TextureKey -> Maybe Texture
get atlas objTyp =
    let
        objKey = gameObjectTypeToInt objTyp
    in
        Dict.get objKey atlas