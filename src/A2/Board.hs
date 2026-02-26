{-
Q3 Explanation:
The Board type represents the 2048 game board together with a random number
generator so that gameplay is deterministic. Sliding logic follows the 2048
rules for moving and merging tiles, and Quadruples are used directly instead
of converting to lists. New tiles are spawned using the stored RNG. The
refreshBoard function resets the board while keeping the same RNG, and
freshBoard initializes a new game with two starting tiles. The isGameOver
function checks whether any slide can still change the board.
-}

module A2.Board
  ( Board,
    Grid,
    grid,
    freshBoard,
    refreshBoard,
    isGameOver,
    emptySpots,
    SlideDirection (..),
    slide,
  )
where

import A2.Grid4x4 (Grid4x4, at, indices, transpose, update)
import A2.Quadruple (Index, Quadruple (..), reverseQuad)
import Numeric.Natural (Natural)
import System.Random (StdGen, randomR)
import Prelude hiding (Left, Right)

-- | A single row of the board is a 'Quadruple' of 'Word's.
type Row = Quadruple Word

-- | The 2048 grid is a \(4\times{}4\) matrix of unsigned integers.
type Grid = Grid4x4 Word

-- | The opaque game state containing the current grid and a random number
-- generator. The RNG allows for deterministic replays.
data Board = Board
  { grid :: Grid,
    rng :: StdGen
  }
  deriving (Eq, Show)

-- | The 4 possible directions a player can slide tiles.
data SlideDirection = SUp | SDown | SLeft | SRight
  deriving (Eq, Show)

compactRow :: Row -> Row
compactRow =
  ntimes 3 step
  where
    step (Quad 0 b c d) = Quad b c d 0
    step (Quad a 0 c d) = Quad a c d 0
    step (Quad a b 0 d) = Quad a b d 0
    step q              = q

mergeRowLeft :: Row -> Row
mergeRowLeft (Quad a b c d)
  | a /= 0 && a == b = Quad (a + b) c d 0
  | b /= 0 && b == c = Quad a (b + c) d 0
  | c /= 0 && c == d = Quad a b (c + d) 0
  | otherwise        = Quad a b c d

slideRowLeft :: Row -> Row
slideRowLeft =
  compactRow . mergeRowLeft . compactRow

slideRowRight :: Row -> Row
slideRowRight =
  reverseQuad . slideRowLeft . reverseQuad

slideGrid :: SlideDirection -> Grid -> Grid
slideGrid SLeft g =
  fmap slideRowLeft g

slideGrid SRight g =
  fmap slideRowRight g

slideGrid SUp g =
  transpose (slideGrid SLeft (transpose g))

slideGrid SDown g =
  transpose (slideGrid SRight (transpose g))



-- | Initialize a fresh 2048 game board, a 'Grid' of 0s with two tiles (2s or
-- 4s) placed randomly on the board.
freshBoard :: StdGen -> Board
freshBoard gen =
  spawnTile (spawnTile empty)
  where
    empty = Board
      { grid = pure (pure 0)
      , rng  = gen
      }

-- | Resets the game board, preserving the RNG.
refreshBoard :: Board -> Board
refreshBoard (Board _ gen) =
  freshBoard gen


-- | Finds all 'empty' positions on a board (i.e., spots where the tile value is
-- 0).
emptySpots :: Board -> [(Index, Index)]
emptySpots b =
  [ xy | xy <- indices, at (grid b) xy == 0 ]

-- | Checks if no further slides on a 'Board' change the board. Alternatively
-- understood, checks if the board is full and no adjacent tiles can be merged.
isGameOver :: Board -> Bool
isGameOver b =
  all noChange [SUp, SDown, SLeft, SRight]
  where
    noChange dir =
      let (_, changed) = slide b dir
      in not changed

-- | Performs a row slide action in a specific direction, returning:
--   
-- 1. The resulting 'Board' of the slide with a randomly spawned tile.
-- 2. A 'Bool' indicating if the slide affected the board in any way.
slide :: Board -> SlideDirection -> (Board, Bool)
slide b dir =
  let newGrid = slideGrid dir (grid b)
  in if newGrid == grid b
       then (b, False)
       else
         let b' = b { grid = newGrid }
         in (spawnTile b', True)

-- | Randomly spawns a tile on a board, with a 90% chance of being a 2 and a 10%
-- chance of being a 4. Only spawns a tile if there is at least 1 available spot
-- on the board.
spawnTile :: Board -> Board
spawnTile b =
  case emptySpots b of
    [] -> b
    spots ->
      let (i, gen1) = randomR (0, length spots - 1) (rng b)
          pos       = spots !! i
          (r, gen2) = randomR (1 :: Int, 10) gen1
          val       = if r == 1 then 4 else 2
      in Board
           { grid = update pos val (grid b)
           , rng  = gen2
           }

ntimes :: Natural -> (a -> a) -> (a -> a)
ntimes 0 _ = id
ntimes n f = f . ntimes (n - 1) f
