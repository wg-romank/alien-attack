module GameState exposing (GameState, Position, initialState, PlayerAction(..), registerUserInput, step, widthFloat, heightFloat, enemiesRoll, EnemyAction(..))

import Random
import Math.Vector2 as Vec2 exposing (vec2, Vec2)

type alias Position = {
        pos: Vec2,
        width: Float,
        height: Float,
        sinceSpawned: Float
    }

newPosition: Vec2 -> Float -> Float -> Position
newPosition pos width height = { pos = pos, width = width, height = height, sinceSpawned = 0.0 }

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
    PlayerMove (Float, Float) | PlayerFire | PlayerMoveLeft | PlayerMoveRight

type EnemyAction = Move | Attack

rollEnemyAction: Random.Generator EnemyAction
rollEnemyAction = Random.weighted (100, Move) [(0, Attack)]

type alias GameState = {
        fuel: Int,
        horizontalSpeed: Float,
        userInput: List PlayerAction,
        bgOffset: Float,
        boardSize: Size,
        playerPosition: Position,
        enemies: List Position,
        rounds: List Position,
        enemyRounds: List Position,
        enemyRoll: List (EnemyAction)
    }

spawnRound: Position -> Position
spawnRound player = newPosition player.pos 2.0 4.0 |> moveX -7.0

spawnEnemyRound: Position -> Position
spawnEnemyRound enemy = newPosition enemy.pos 4.0 4.0 |> moveX -13.0 |> moveY -32.0

initialState: GameState
initialState = {
        fuel = 1000,
        horizontalSpeed = 0,
        userInput = [],
        bgOffset = 10000,
        boardSize = { width = 160, height = 240 },
        playerPosition = newPosition (vec2 72 222) 16.0 16.0,
        enemies = [ newPosition (vec2 56 24) 32.0 32.0 ],
        rounds = [],
        enemyRounds = [],
        enemyRoll = []
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

-- positonFromSpeed: Vec2 -> Float -> Float -> Vec2
-- positonFromSpeed previousPos speed delta =
--     let


playerMoveHorizontal: Int -> GameState -> GameState
playerMoveHorizontal value state =
    let
        previousPosition = state.playerPosition.pos
        newPlayerPosition = Vec2.add previousPosition (vec2 (value |> toFloat) 0)
    in
        playerMove newPlayerPosition { state | fuel = state.fuel - value }
    


performPlayerAction: List PlayerAction -> GameState -> GameState
performPlayerAction action state =
    case action of
        f :: rest ->
            { state | userInput = rest } |>
            case f of
                -- TODO: move with constant speed on touch
                PlayerMove (x, y) -> playerMove (vec2 x y)
                PlayerMoveLeft -> playerMoveHorizontal -10
                PlayerMoveRight -> playerMoveHorizontal 10
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

enemyMove: Float -> Position -> GameState -> GameState
enemyMove delta enemy state = state
    -- let
    --     width = widthFloat state.boardSize
    --     height = heightFloat state.boardSize
    -- in
    --     { state | 
    --         enemies = List.map (\e -> 
    --             if e == enemy then
    --                 { enemy | pos = 
    --                     ( enemy |> moveY -(delta / 200.0) ).pos
    --                     |> warpCoordinates (width - enemy.width) (height - enemy.height) }
    --             else e
    --         ) state.enemies
    --     }

enemyAttack: Position -> GameState -> GameState
enemyAttack enemy state = { state | enemyRounds = state.enemyRounds ++ [spawnEnemyRound enemy] }

performEnemyAction: Float -> Position -> EnemyAction -> GameState -> GameState
performEnemyAction delta enemy action state =
    case action of
        Move -> enemyMove delta enemy state
        Attack -> enemyAttack enemy state

performEnemiesActions: Float -> GameState -> GameState
performEnemiesActions delta state =
    let
        -- TODO: potential bug in keys enemy that rolls killed?
        enemiesWithActions = List.map2 (\a b -> (a, b)) state.enemies state.enemyRoll
    in
        List.foldl (\(pos, action) ss -> performEnemyAction delta pos action ss)
        state
        enemiesWithActions

enemiesRoll: GameState -> Random.Generator (List EnemyAction)
enemiesRoll state = Random.list (List.length state.enemies) rollEnemyAction


moveRound: Float -> Position -> GameState -> GameState
moveRound delta round state =
    let
        enemiesAlive = state.enemies |> List.filter (\e -> intersect e round |> not )
        enemiesHit = state.enemies |> List.filter (\e -> intersect e round )
        rounds = List.filterMap (\rr ->
            if rr == round then
                if List.isEmpty enemiesHit && Vec2.getY round.pos > 0 then
                    Just ( round |> moveY (delta / 10.0) )
                else Nothing
            else Just rr) state.rounds
    in
        { state | rounds = rounds, enemies = enemiesAlive }

moveRounds: Float -> GameState -> GameState
moveRounds delta state = List.foldl (moveRound delta) state state.rounds

moveEnemyRounds: Float -> GameState -> GameState
moveEnemyRounds delta state = {
        state | enemyRounds =
            List.filterMap (\r ->
                if Vec2.getY r.pos > heightFloat state.boardSize then Nothing
                else Just (r |> moveY (-delta / 20.0)) ) state.enemyRounds
    }

registerUserInput: PlayerAction -> GameState -> GameState
registerUserInput action state = { state | userInput = state.userInput ++ [action] }

updateTimeSinceSpawned: Float -> Position -> Position
updateTimeSinceSpawned delta pos = { pos | sinceSpawned = pos.sinceSpawned + delta }

updateTimesSinceSpawned: Float -> GameState -> GameState
updateTimesSinceSpawned delta state = {
        state |
            playerPosition = updateTimeSinceSpawned delta state.playerPosition,
            enemies = List.map (updateTimeSinceSpawned delta) state.enemies,
            rounds = List.map (updateTimeSinceSpawned delta) state.rounds
    }

-- enemySpawn: GameState -> GameState
-- enemySpawn state =


moveBackground: Float -> GameState -> GameState
moveBackground delta state = { state | bgOffset = state.bgOffset + delta }


step: Float -> GameState -> GameState
step timeDelta state =
        state
            |> performPlayerAction state.userInput
            |> moveRounds timeDelta
            |> performEnemiesActions timeDelta
            |> moveEnemyRounds timeDelta
            |> updateTimesSinceSpawned timeDelta
            |> moveBackground timeDelta

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