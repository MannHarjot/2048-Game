{-
Q6 Explanation:
The bot uses a simple first-valid-move strategy. For each turn, it tries slide
directions in a fixed order and chooses the first one that actually changes
the board. This prevents the bot from making useless moves and helps keep the
game progressing. The strategy is simple, deterministic, and easy to
understand. More advanced strategies were considered, but this approach was
chosen to keep the bot lightweight and reliable.

References:
- The 2048 game was played online (https://play2048.co/) to understand the rules,
  how merges work, and the general gameplay experience.
- The GitHub project https://github.com/mmcguill/haskell-2048 was referenced to
  see an example approach to implementing 2048 in Haskell. No code was directly
  copied; it was used for general understanding of how to structure a Haskell
  solution.
- Stack Overflow, Haskell documentation, and other public resources were
  referenced for language features, typeclasses, and debugging ideas.
- ChatGPT was used to help debug compiler errors, understand Safe Haskell
  behavior, and clarify Haskell syntax and design decisions.
-}

module A2.Bot
  ( MyBot,
    botInfoMsg,
    initBot,
    nextMove,
    slide,
  )
where

import A2.Board (Board, SlideDirection (..), slide)

newtype MyBot = MyBot {lastMove :: SlideDirection}
   deriving (Eq, Show)

-- | Gets a human-readable description of the bot with any notable state
-- information.
botInfoMsg :: MyBot -> String
botInfoMsg _ = "First Valid Move"

-- | Initializes a 'MyBot' using the current board state.
initBot :: Board -> MyBot
initBot _ = MyBot SLeft

-- | Calculates the next move a bot should make.
nextMove :: Board -> MyBot -> (MyBot, SlideDirection)
nextMove b (MyBot sd) = try [SUp, SLeft, SDown, SRight]
  where
    try (d:ds) =
      let (_, changed) = slide b d
       in if changed then (MyBot d, d) else try ds
    try [] = (MyBot sd, sd)

