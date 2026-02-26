-- | See https://hackage-content.haskell.org/package/tasty-hunit-0.10.2/docs/Test-Tasty-HUnit.html
-- for documentation on writing unit tests.
module Main where

import A2.Board
import A2.Bot
import A2.Game as Game
import A2.Grid4x4 as G
import A2.Quadruple as Q
import System.Random (mkStdGen)

import Test.Tasty
import Test.Tasty.HUnit

quadrupleTests :: TestTree
quadrupleTests =
  testGroup "Quadruple"
  [ testCase "getQuad accesses correct elements" $ do
      let q = Quad 10 20 30 40
      getQuad I0 q @?= 10
      getQuad I1 q @?= 20
      getQuad I2 q @?= 30
      getQuad I3 q @?= 40

  , testCase "setQuad replaces only the selected index" $ do
      let q = Quad 1 2 3 4
      setQuad I0 9 q @?= Quad 9 2 3 4
      setQuad I1 9 q @?= Quad 1 9 3 4
      setQuad I2 9 q @?= Quad 1 2 9 4
      setQuad I3 9 q @?= Quad 1 2 3 9

  , testCase "reverseQuad reverses element order" $ do
      reverseQuad (Quad 1 2 3 4) @?= Quad 4 3 2 1

  , testCase "indices enumerates all Quadruple indices" $ do
      Q.indices @?= [I0, I1, I2, I3]

  , testCase "Foldable foldr sums elements left-to-right" $ do
      sum (Quad 1 2 3 4) @?= 10
  ]

testGrid :: Grid4x4 Int
testGrid = 
  Quad
    (Quad  1  2  3  4)
    (Quad  5  6  7  8)
    (Quad  9 10 11 12)
    (Quad 13 14 15 16)

grid4x4Tests :: TestTree
grid4x4Tests =
  testGroup "Grid4x4"
  [ testCase "at accesses correct coordinates" $ do
      at testGrid (I0, I0) @?= 1
      at testGrid (I1, I2) @?= 7
      at testGrid (I3, I3) @?= 16

  , testCase "update replaces exactly one cell" $ do
      let g' = G.update (I1, I1) 99 testGrid
      at g' (I1, I1) @?= 99
      at g' (I1, I0) @?= 5
      at g' (I0, I1) @?= 2

  , testCase "indices enumerates all 16 coordinates in row-major order" $ do
      length G.indices @?= 16
      head G.indices @?= (I0, I0)
      last G.indices @?= (I3, I3)

  , testCase "transpose swaps rows and columns" $ do
      let t = transpose testGrid
      at t (I0, I1) @?= at testGrid (I1, I0)
      at t (I2, I3) @?= at testGrid (I3, I2)

  , testCase "transpose is involutive (transpose . transpose = id)" $ do
      transpose (transpose testGrid) @?= testGrid
  ]



testBoard :: Board
testBoard = freshBoard (mkStdGen 42)



boardTests :: TestTree
boardTests =
  testGroup "Board"
  [ testCase "emptySpots on fresh board returns 14 spots" $ do
      length (emptySpots testBoard) @?= 14

  , testCase "refreshBoard resets grid to empty" $ do
      let b' = refreshBoard testBoard
      length (emptySpots b') @?= 14

  , testCase "slide returns True when a move is possible" $ do
      let (_, changed) = slide testBoard SLeft
      changed @?= True

  , testCase "isGameOver is False on a fresh board" $ do
      isGameOver testBoard @?= False
  ]


gameTests :: TestTree
gameTests =
  testGroup "Game"
  [ testCase "Begin starts a new human game" $ do
      let g = Game.update (Begin (mkStdGen 1)) Quitted
      case g of
        Playing Human _ -> pure ()
        _ -> assertFailure "Begin did not start a human game"

  , testCase "ToggleBot switches Human to Bot" $ do
      let g0 = Game.update (Begin (mkStdGen 1)) Quitted
          g1 = Game.update ToggleBot g0
      case g1 of
        Playing (Bot _) _ -> pure ()
        _ -> assertFailure "ToggleBot did not switch to Bot"

  , testCase "ToggleBot switches Bot back to Human" $ do
      let g0 = Game.update (Begin (mkStdGen 1)) Quitted
          g1 = Game.update ToggleBot g0
          g2 = Game.update ToggleBot g1
      case g2 of
        Playing Human _ -> pure ()
        _ -> assertFailure "ToggleBot did not switch back to Human"

  ,testCase "Quit always transitions to Quitted" $ do
      let g0 = Game.update (Begin (mkStdGen 1)) Quitted
          g1 = Game.update Quit g0
      case g1 of
        Quitted -> pure ()
        _       -> assertFailure "Quit did not transition to Quitted"

  ]


botTests :: TestTree
botTests =
  testGroup "Bot"
  [ testCase "initBot creates a bot" $ do
        let _ = initBot (freshBoard (mkStdGen 1))
        assertBool "Bot initialized" True


  , testCase "nextMove returns a valid direction" $ do
      let b = freshBoard (mkStdGen 1)
          bot = initBot b
          (_, dir) = nextMove b bot
      case dir of
        SUp    -> pure ()
        SDown  -> pure ()
        SLeft  -> pure ()
        SRight -> pure ()

  , testCase "nextMove chooses a move that changes the board when possible" $ do
      let b = freshBoard (mkStdGen 1)
          bot = initBot b
          (_, dir) = nextMove b bot
          (_, changed) = slide b dir
      assertBool "Bot chose an invalid move" changed
  ]



main :: IO ()
main = defaultMain $
  testGroup "A2"
  [ quadrupleTests
  , grid4x4Tests
  , boardTests
  , gameTests
  , botTests
  ]
