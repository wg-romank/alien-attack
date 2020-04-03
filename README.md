# Alien Attack arcade-style game

This is a tech test / demo for working with WegbGL from Elm programming language.

Main motivation behind this project was to explore functional programming techniques
and how well they would fit with game development.

This repo implements basic concepts like working with textures, defining GLSL shaders for WebGL
and doing simple math to run game simulation as well as minor HUD bits and scaling for different screens.

Implementation is rather naive and could be improved in terms of efficiency by utilizing
common space segregation data structures like VP or Octo trees. (At the moment collision detection
is done by doing all-vs-all objects match which would obviously not work for any decently sized game).

Turns out functional style and immutable data structures are nice fit for the task, just look at the main game loop

```elm
gameLoop: Float -> GameState -> GameState
gameLoop timeDelta state =
        state
            |> performPlayerAction state.userInput
            |> moveRounds timeDelta
            |> performEnemiesActions timeDelta
            |> moveEnemyRounds timeDelta
            |> updateTimesSinceSpawned timeDelta
            |> playerMoveFromCourse timeDelta
            |> moveBackground
            |> enemySpawn
```

Also HTML5 programming model for working with animation that is using time `delta`
would be familiar for people with experience working with popular game engines like Godot and Unity.

In the end there are maybe simplier and faster ways to implement similar functionality using one of popular
game engines. Current approach might be interesting for person familiar with web or functional programming or both.
Main benefit here is explicit state management. Since all data structures are immutable, any new updates need
to go through explicit queuing which helps to reason about the logic. (See user input handling or random generation)

Game is hosted as static page at the url in the barcode.

<img src="https://wg-romank.github.io/alien-attack/assets/alien-attack-url-barcode.png" width="128" height="128"/>

[https://wg-romank.github.io/alien-attack/]
