import Data.List
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main
        = play
          (InWindow
          "Nice Window"
          (1000, 1000)
          (0, 0)) 
        white
        50
        initWorld
        pictures
        (\e -> \p -> p) -- no handler
        (\t -> \w -> w) -- no timestep

{- Types:
tangles: e.g. 10***.3:1.1.1.3.-4......
(polyhedra)

Data:
Basic tangles 0, infinity, -1, 1
Basic polyhedron 1*

Functions I need:
twist :: nat -> Picture (or [Picture]?)

reverseCrossings :: Picture -> Picture ([Picture] -> [Picture])

rotate90CW :: Picture -> Picture ([Picture] -> [Picture])

connectTangles :: Picture -> Picture -> Picture

embedInPolyhedron :: Picture -> polyhedron -> Picture 
(later, multiple tangles = [Picture] -> labeled polyhedron -> Picture)

renderTangle :: tangle -> Picture

renderKnot :: [tangle] -> polyhedron -> Picture
or (conway notation = (Polyhedron, [Tangle]) where polyhedron is a number
that indexes a list of the polyhedron pictures / draws the polyhedron around
the tangles, and Tangle is a list of numbers with combinators like -, ,?
-}

type Twist = Int
type Polyhedron = Int
type ConwayNotation = ([Twist], Polyhedron)

trefoil :: ConwayNotation
trefoil = ([3], 1)

figure8 :: ConwayNotation
figure8 = ([2, 2], 1)

stevedore :: ConwayNotation
stevedore = ([4, 2], 1)

six_2 :: ConwayNotation
six_2 = ([3, 1, 2], 1)

six_3 :: ConwayNotation
six_3 = ([2, 1, 1, 2], 1)

-----------

-- Code to generate knots and filter out (lower-crossing knots / links / dups)

-- given # crossings, produce all knots with that # crossings
dowkers :: Int -> [[(Int, Int)]]
dowkers n
  | n <= 0 = []
  | otherwise =
  let (odds, evens) = (take n [1, 3 ..], take n [2, 4 ..]) in
  map (zip odds) (permutations evens)

partitions :: Int -> [[Int]]
partitions n
  | n <= 0 = [[]]
  | otherwise =
  let choose n = [1, 2 .. n] in
  let subpartition y = map (y :) $ partitions (n - y) in
  concatMap subpartition (choose n)


-----------

foo :: [Picture]
foo = [Circle 200]

embedIn :: Polyhedron -> [Picture] -> [Picture]
embedIn polyhedron tanglePic = foo

-- should probably return other stuff like where the relevant endpoints are
renderTangle :: [Twist] -> [Picture]
renderTangle t = foo
-- let multiply a b = a :: [b] in {- is there an identity for mult? -}
             -- foldl multiply [0] . map renderTwist

renderKnot :: ConwayNotation -> [Picture]
renderKnot (tangle, polyhedron) = embedIn polyhedron $ renderTangle tangle

-- TODO: commas, polyhedra

initWorld :: [Picture]
initWorld = renderKnot trefoil

-- WHEREIN: a series of confusing and unpleasant operations happens to data
testTwist :: [Picture]
testTwist = let thickness = 15 in
          let twist1 = map (rotate 70) [ThickArc 0 160 100 thickness {-, ThickArc 0 160 (100 + thickness) thickness -}] in
          let twist2 = map (translate 0 200 . rotate 70) 
                       [ThickArc 180 340 100 thickness {-, ThickArc 180 340 (100 + thickness) thickness -}] in
          let twist = twist1 ++ twist2 in
          let x = 0.0 in
          let y = 220.0 in
          let init = 500.0 in
          let twistall = concatMap (\(y, t) -> map (translate x y) t) $
                         zip [init, (init - y) ..] (replicate 5 twist) in
          map (translate 0 (-80) . scale 0.5 0.5) twistall

              -- This lack of flipping is sort of a problem
              -- need a better haskell emacs-mode (or switch to OCaml graphics -- has beziers)
              -- To reverse the crossings, flip the image (will have to hard-code twists with different angles? or change their angles)
