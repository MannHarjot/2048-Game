{-
Q4 Explanation:
The game engine follows the Model-View-Update pattern. The Game type represents
all possible states of the application, including playing, game over, and
quitting. The update function handles all state transitions based on messages
such as sliding, restarting, toggling the bot, or quitting. Restarting the
game always creates a new game with a human player.
-}

{-# LANGUAGE GADTs #-}

module A2.Game
  ( Game (..),
    initGame,
    Message (..),
    update,
    Player (..),
  )
where

import A2.Board
  ( Board,
    SlideDirection (..),
    freshBoard,
    isGameOver,
    refreshBoard,
    slide,
  )
import A2.Bot (MyBot, botInfoMsg, initBot, nextMove)
import System.Random (StdGen)

-- | The active player entity. Either a Human or a Bot (with an internal
-- memory).
data Player where
  Human :: Player
  Bot :: MyBot -> Player

instance Show Player where
  show Human = "Human"
  show (Bot t) = "Bot (" ++ botInfoMsg t ++ ")"

-- | A 2048 game has 3 possible states: playing, waiting for restart, and
-- shutting down.
data Game
  = Playing Player Board
  | GameOver Board
  | Quitted
  deriving (Show)

-- | Initialize a 2048 game.
initGame :: StdGen -> Game
initGame = Playing Human . freshBoard

-- | All possible actions a player (or bot) can make in a game of 2048.
data Message
  = Begin StdGen
  | Slide SlideDirection
  | ToggleBot
  | BotStep
  | Restart
  | Quit
  deriving (Show)

-- | Updates the game model according to actions a user (or bot) make.
update :: Message -> Game -> Game
update (Begin gen) _ = Playing Human (freshBoard gen)
update ToggleBot (Playing Human b) =  Playing (Bot (initBot b)) b
update ToggleBot (Playing (Bot _) b) = Playing Human b
update BotStep (Playing (Bot bot) b) = 
    let (bot', dir) = nextMove b bot
        (b', changed) = slide b dir
    in if not changed
       then Playing (Bot bot') b
       else if isGameOver b'
              then GameOver b'
              else Playing (Bot bot') b'
update (Slide dir) (Playing Human b) =
    let (b', changed) = slide b dir
    in if not changed
       then Playing Human b
       else if isGameOver b'
              then GameOver b'
              else Playing Human b'
update Restart (Playing _ b) =
  Playing Human (refreshBoard b)

update Restart (GameOver b) =
  Playing Human (refreshBoard b)


update Quit _ = Quitted
update _ g = g
