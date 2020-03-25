module GameState exposing (GameState, initialState, PlayerAction(..), registerUserInput, step, widthFloat, heightFloat)

import Math.Vector2 as Vec2 exposing (vec2, Vec2)

type alias Position = {
        pos: Vec2,
        width: Float,
        height: Float
    }

moveX: Float -> Position -> Position
moveX amount p = { p | pos = Vec2.sub p.pos (vec2 amount 0.0) }

moveY: Float -> Position -> Position
moveY amount p = { p | pos = Vec2.sub p.pos (vec2 0.0 amount) }

moveTo: Position -> Vec2 -> Position
moveTo p to = { p | pos = to }

type alias Size = {
        width: Int,
        height: Int
    }

widthFloat: Size -> Float
widthFloat size = toFloat size.width

heightFloat: Size -> Float
heightFloat size = toFloat size.height

type PlayerAction =
    PlayerMove (Float, Float) | PlayerFire

type alias GameState = {
        userInput: List PlayerAction,
        boardSize: Size,
        playerPosition: Position,
        enemies: List Position,
        rounds: List Position
    }

spawnRound: Position -> Position
spawnRound player = {
        pos = player.pos,
        width = 2.0,
        height = 4.0
    } |> moveX -7.0

initialState: GameState
initialState = {
        userInput = [],
        boardSize = { width = 160, height = 240 },
        playerPosition = { pos = vec2 72 224, width = 16.0, height = 16.0 },
        enemies = [ { pos = vec2 56 24, width = 32.0, height = 32.0} ],
        rounds = []
    }

playerFire: GameState -> GameState
playerFire state = { state | rounds = state.rounds ++ [spawnRound state.playerPosition] }


-- TODO: FIX
playerMove: Vec2 -> GameState -> GameState
playerMove to state =
        let
            width = widthFloat state.boardSize
            height = heightFloat state.boardSize
            playerWidth = state.playerPosition.width
            playerHeight = state.playerPosition.height
        in
            Vec2.toRecord to
            |> \p -> vec2
                        ( clamp 0.0 (width - playerWidth) p.x )
                        ( clamp ( 2.0 * (height - playerHeight) / 3.0 ) (height - playerHeight) p.y )
            |> \q -> { state| playerPosition = moveTo state.playerPosition q }


performPlayerAction: List PlayerAction -> GameState -> GameState
performPlayerAction action state =
    case action of
        f :: rest ->
            { state | userInput = rest } |>
            case f of
                PlayerMove (x, y) -> playerMove (vec2 x y)
                PlayerFire -> playerFire
        [] -> state


modFloat: Float -> Float -> Float -> Float
modFloat from to n =
    if n > to then from + (n - to)
    else if n < from then to - n
        else n

warpCoordinates: Float -> Float -> Vec2 -> Vec2
warpCoordinates w h v =
    vec2 
        (Vec2.getX v |> modFloat 0 w)
        (Vec2.getY v |> modFloat 0 h)

enemyMove: Float -> Float -> Float -> Position -> Position
enemyMove delta width height enemy = 
    { enemy | pos = 
        ( enemy |> moveY -(delta / 200.0) ).pos
        |> warpCoordinates (width - enemy.width) (height - enemy.height) }

moveEnemies: Float -> GameState -> GameState
moveEnemies delta state =
        let
            width = widthFloat state.boardSize
            height = heightFloat state.boardSize
        in
            { state | enemies = List.map (enemyMove delta width height) state.enemies }


moveRound: Float -> GameState -> Position -> List Position
moveRound delta state round =
    if Vec2.getY round.pos <= 0 then []
    else [round |> moveY 1.0]

moveRounds: Float -> GameState -> GameState
moveRounds delta state =
        { state | rounds =
            List.map (moveRound delta state) state.rounds
                |> List.concat
        }

registerUserInput: PlayerAction -> GameState -> GameState
registerUserInput action state = { state | userInput = state.userInput ++ [action] }

step: Float -> GameState -> GameState
step timeDelta state =
        state
            |> performPlayerAction state.userInput
            |> moveEnemies timeDelta
            |> moveRounds timeDelta


            -- TODO: enemy spawn
            -- let enemySpawn = List.map
            --         (\t -> newEnemy (vec2 x y) t)
            --         (ME.toList (Dict.get (gameObjectTypeToInt Enemy) model.atlas))
            -- in
            --     if model.from == vec2 x y then
            --         ({
            --             model |
            --             message = "Detecetd Tap" ++ (Debug.toString (vec2 x y)),
            --             objects = model.objects ++ enemySpawn
            --         }, Cmd.none)
            --     else (model, Cmd.none)


intersect: Position -> Position -> Bool
intersect a b = abs (Vec2.getX a.pos + a.width / 2.0 - Vec2.getX b.pos + b.width / 2.0) <= (max a.width b.width) / 2.0 &&
        abs (Vec2.getY a.pos + a.height / 2.0 - Vec2.getY b.pos + b.width / 2.0) <= (max a.height b.height) / 2.0