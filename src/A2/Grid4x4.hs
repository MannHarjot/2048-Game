{-
Q2 Explanation:
The 4×4 grid is represented as a Quadruple of Quadruples in row-major order.
This keeps the grid size fixed and avoids out-of-bounds errors. The at and
update functions safely access and modify grid cells using index pairs.
The indices function lists all valid coordinates in row-major order, which
is useful for iteration. The transpose function swaps rows and columns without
converting the grid into lists.
-}

module A2.Grid4x4
  ( Grid4x4,
    at,
    update,
    indices,
    transpose,
  )
where

import A2.Quadruple (Index (..), Quadruple (..), getQuad, setQuad)
import qualified A2.Quadruple as Q (indices)

-- | A /row-major/ 4x4 grid.
type Grid4x4 a = Quadruple (Quadruple a)

-- | Accessor for the element at the \((x,y)\) coordinate.
at :: Grid4x4 a -> (Index, Index) -> a
at g (i, j) = getQuad j (getQuad i g)

-- | Returns a new 'Grid4x4' with the value at \((x,y)\) replaced.
update :: (Index, Index) -> a -> Grid4x4 a -> Grid4x4 a
update (i, j) val g =
  setQuad i newRow g
  where
    row    = getQuad i g
    newRow = setQuad j val row

-- | Enumerates all coordinates (in row-major order).
indices :: [(Index, Index)]
indices = [ (i, j) | i <- Q.indices, j <- Q.indices ]

-- | Matrix transpose operation. Swaps values such that \(M_{ij}\) becomes
-- \(M_{ji}\).
transpose :: Grid4x4 a -> Grid4x4 a
transpose q = 
  Quad
    (Quad (at q (I0,I0)) (at q (I1,I0)) (at q (I2,I0)) (at q (I3,I0)))
    (Quad (at q (I0,I1)) (at q (I1,I1)) (at q (I2,I1)) (at q (I3,I1)))
    (Quad (at q (I0,I2)) (at q (I1,I2)) (at q (I2,I2)) (at q (I3,I2)))
    (Quad (at q (I0,I3)) (at q (I1,I3)) (at q (I2,I3)) (at q (I3,I3)))
