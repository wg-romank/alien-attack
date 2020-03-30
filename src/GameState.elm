module GameState exposing (..)

import Random
import Math.Vector2 as Vec2 exposing (vec2, Vec2)

type alias Position = {
        pos: Vec2,
        width: Float,
        height: Float,
        sinceSpawned: Float
    }

newPosition: Float -> Float -> Vec2 -> Position
newPosition width height pos = { pos = pos, width = width, height = height, sinceSpawned = 0.0 }

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

type PlayerAction = PlayerFire | PlayerMoveLeft | PlayerMoveRight

type EnemyAction = Move | Attack

rollEnemyAction: Random.Generator EnemyAction
rollEnemyAction = Random.weighted (99, Move) [(1, Attack)]

type alias GameState = {
        score: Int,
        playerDead: Bool,
        playerWon: Bool,
        fuel: Float,
        course: Float,
        horizontalSpeed: Float,
        userInput: List PlayerAction,
        bgOffset: Float,
        boardSize: Size,
        playerPosition: Position,
        enemies: List Position,
        rounds: List Position,
        maxEnemiesToSpawn: Int,
        enemyRounds: List Position,
        enemyRoll: List (EnemyAction),
        enemySpawnRoll: List (Vec2)
    }

spawnRound: Position -> Position
spawnRound player = newPosition 2.0 4.0 player.pos |> moveX -7.0

spawnEnemyRound: Position -> Position
spawnEnemyRound enemy = newPosition 4.0 4.0 enemy.pos |> moveX -13.0 |> moveY -32.0

initialState: GameState
initialState = {
        score = 0,
        playerDead = False,
        playerWon = False,
        fuel = 100,
        course = 0,
        horizontalSpeed = 0,
        userInput = [],
        bgOffset = 10000,
        boardSize = { width = 160, height = 240 },
        playerPosition = newPosition 16.0 16.0 (vec2 72 222),
        enemies = [ newPosition 32.0 32.0 (vec2 56 24) ],
        maxEnemiesToSpawn = 5,
        rounds = [],
        enemyRounds = [],
        enemyRoll = [],
        enemySpawnRoll = []
    }

isOver: GameState -> Bool
isOver state = state.playerDead || state.playerWon

playerFire: GameState -> GameState
playerFire state = { state | rounds = state.rounds ++ [spawnRound state.playerPosition] }


playerAdjustCourse: Float -> GameState -> GameState
playerAdjustCourse value state =
    let
        budget = abs value
    in
        if state.fuel >= budget then
        { state | fuel = state.fuel - budget, course = state.course + value  }
        else state
    

performPlayerAction: List PlayerAction -> GameState -> GameState
performPlayerAction action state =
    case action of
        f :: rest ->
            { state | userInput = rest } |>
            case f of
                -- TODO: move with constant speed on touch
                PlayerMoveLeft -> playerAdjustCourse -10
                PlayerMoveRight -> playerAdjustCourse 10
                PlayerFire -> playerFire
        [] -> state

playerMoveFromCourse: Float -> GameState -> GameState
playerMoveFromCourse delta state =
    let
        previousPosition = state.playerPosition
        nPos = Vec2.add previousPosition.pos (vec2 (state.course * delta / 1000) 0) 
        width = widthFloat state.boardSize
        playerDeorbited = Vec2.getX nPos |> (\x -> x < 0 || x > width - state.playerPosition.width)
    in
        { state | playerPosition = { previousPosition | pos = nPos }, playerDead = playerDeorbited }


enemyMove: Float -> Position -> GameState -> GameState
enemyMove delta enemy state =
    let
        height = heightFloat state.boardSize
        enemiesMoved = 
            List.filterMap (\e -> 
                if e == enemy then
                    let
                        nPos = ( enemy |> moveY -(delta / 200.0) ).pos
                    in
                        if Vec2.getY nPos > height then Nothing
                        else Just { enemy | pos = nPos }
                else Just e
            ) state.enemies
        enemiesHitPlayer =
            List.map (\e -> intersect e state.playerPosition) enemiesMoved
             |> List.filter identity
    in
        { state | enemies = enemiesMoved, playerDead = enemiesHitPlayer |> List.isEmpty |> not }

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

enemySpawnRoll: GameState -> Random.Generator (List Vec2)
enemySpawnRoll state =
    let
        nEnemies = Random.weighted (99, 0) [(1, state.maxEnemiesToSpawn)]
        -- TODO: fix hardcoded
        enemyCoordinates = Random.int 1 (state.boardSize.width - 16) |> Random.map (\v -> vec2 (toFloat v) 24)
    in
        nEnemies |> Random.andThen (\len -> Random.list len enemyCoordinates)


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
        { state | rounds = rounds, enemies = enemiesAlive, score = state.score + List.length enemiesHit }

moveRounds: Float -> GameState -> GameState
moveRounds delta state = List.foldl (moveRound delta) state state.rounds

moveEnemyRounds: Float -> GameState -> GameState
moveEnemyRounds delta state =
    let
        newRounds =
            List.filterMap (\r ->
                if Vec2.getY r.pos > heightFloat state.boardSize then Nothing
                else Just (r |> moveY (-delta / 20.0)) ) state.enemyRounds
        playerHitByRound = List.filter (\r -> intersect r state.playerPosition) newRounds |> List.isEmpty |> not
    in
    { state | enemyRounds = newRounds, playerDead = playerHitByRound }

registerUserInput: PlayerAction -> GameState -> GameState
registerUserInput action state = { state | userInput = state.userInput ++ [action] }

registerUserTap: (Float, Float) -> GameState -> GameState
registerUserTap (x, _) state =
    let
        posX = Vec2.getX state.playerPosition.pos
    in
        if x < posX - state.playerPosition.width then registerUserInput PlayerMoveLeft state
        else if x < posX + state.playerPosition.width * 2 then registerUserInput PlayerFire state
        else registerUserInput PlayerMoveRight state

updateTimeSinceSpawned: Float -> Position -> Position
updateTimeSinceSpawned delta pos = { pos | sinceSpawned = pos.sinceSpawned + delta }

updateTimesSinceSpawned: Float -> GameState -> GameState
updateTimesSinceSpawned delta state = {
        state |
            playerPosition = updateTimeSinceSpawned delta state.playerPosition,
            enemies = List.map (updateTimeSinceSpawned delta) state.enemies,
            rounds = List.map (updateTimeSinceSpawned delta) state.rounds
    }

doNotIntersect: Position -> List Position -> Bool
doNotIntersect element listOfElements = 
        List.map (\existing -> inVicinity element existing |> not) listOfElements
        |> List.foldl (&&) True

enemySpawn: GameState -> GameState
enemySpawn state =
    let 
        enemySpawns = List.map (newPosition 32.0 32.0) state.enemySpawnRoll
        newEnemies = List.foldl
            ( \l acc ->
                    let
                        noIntersections = doNotIntersect l acc
                    in
                        if noIntersections then acc ++ [l]
                        else acc )
            state.enemies
            enemySpawns
    in
        { state | enemies = newEnemies }


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
            -- |> moveBackground timeDelta
            |> playerMoveFromCourse timeDelta
            |> enemySpawn

inVicinity: Position -> Position -> Bool
inVicinity a b =
    let
        centerA = Vec2.add a.pos (vec2 (a.width / 2.0) (a.height / 2.0) )
        centerB = Vec2.add b.pos (vec2 (a.width / 2.0) (a.height / 2.0) )
    in
        (Vec2.sub centerA centerB |> Vec2.length) <= (max b.height (max a.height (max a.width b.width))) + 2.0

intersect: Position -> Position -> Bool
intersect a b = abs (Vec2.getX a.pos + a.width / 2.0 - Vec2.getX b.pos + b.width / 2.0) <= (max a.width b.width) / 2.0 &&
        abs (Vec2.getY a.pos + a.height / 2.0 - Vec2.getY b.pos + b.width / 2.0) <= (max a.height b.height) / 2.0