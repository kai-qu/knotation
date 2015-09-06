{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Data.List
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- ghc --make knot.hs; ./knot -o knot.svg -h 500; chrome knot.svg

illustrateBézier c1 c2 x2
    =  endpt
    <> endpt  # translate x2
    <> ctrlpt # translate c1
    <> ctrlpt # translate c2
    <> l1
    <> l2
    <> fromSegments [bézier3 c1 c2 x2]
  where
    dashed  = dashingN [0.03,0.03] 0
    endpt   = circle 0.05 # fc red  # lw none
    ctrlpt  = circle 0.05 # fc blue # lw none
    l1      = fromOffsets [c1] # dashed
    l2      = fromOffsets [x2 ^-^ c2] # translate c2 # dashed

x2      = r2 (3,-1) :: V2 Double     -- endpoint
[c1,c2] = map r2 [(1,2), (3,0)]     -- control points

example :: Diagram B
example = illustrateBézier c1 c2 x2

----------------------

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

-- goal for now
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

-- foo :: [Picture]
-- foo = [Circle 200]

-- embedIn :: Polyhedron -> [Picture] -> [Picture]
-- embedIn polyhedron tanglePic = foo

-- -- should probably return other stuff like where the relevant endpoints are
-- renderTangle :: [Twist] -> [Picture]
-- renderTangle t = foo
-- -- let multiply a b = a :: [b] in {- is there an identity for mult? -}
--              -- foldl multiply [0] . map renderTwist

-- renderKnot :: ConwayNotation -> [Picture]
-- renderKnot (tangle, polyhedron) = embedIn polyhedron $ renderTangle tangle

-- -- TODO: commas, polyhedra

-- initWorld :: [Picture]
-- initWorld = renderKnot trefoil

----------------------

{- Notes on drawing:

- Straight lines for Conway instead -- but how to prevent self-intersection?
- Can a spline through every knot point (in order) work? But then how to articulate the crossings? 
- Maybe a spline through each segment. But how to calculate the segments? And how to figure out the extra control points?
- Twists: should only draw (n - 2) of them; the 2 crossings at the end need to be part of the splines that connect to other twists 

To draw: trefoil, [2 1]; [2 1 2]; [3 3 3]... -}

w = 1                           -- total width
h = 4                      -- total height
offCenter = 0.2
pts (baseX, baseY) = 
  let w' = w / 2.0 in
  let h' = h / 4.0 in
  map p2 [(baseX - offCenter, baseY), (baseX - w', baseY - h'),
          (baseX + w', baseY - (h' * 3)), (baseX + offCenter, baseY - h)]
spot = circle 0.02 # fc blue # lw none
mkPath base = position (zip (pts base) (repeat spot))
             <> cubicSpline False (pts base)

twistOffsetX = 0
delta = 0.2
twistOffsetY = h / 2.0 + delta
twistOffsetP n = (twistOffsetX, n * twistOffsetY)

smash xs = foldl (<>) mempty xs

-- twist :: Diagram B
twist n base =
      let translateBy n obj = obj # translate (r2 $ twistOffsetP n) in
      let twists = take n $ zipWith translateBy [0, 1..] (repeat $ mkPath base) in
      let halfTwists = [mempty] in -- [topHalf 0, bottomHalf n] -- TODO
      smash $ twists ++ halfTwists
      -- hcat [mkPath False, mkPath False]
      -- mkPath False ||| mkPath False

cubicSplineEx :: Diagram B
cubicSplineEx = twist 3 (0, 0) # rotateBy (1/4)
              -- TODO: for multiple twists, endpoints + auto-placement +
              -- the no-rotate method?
              -- for figure-8      
              -- (twist 2 (0, 0) ||| (twist 2 (0, 0) # rotateBy (1/4)))
                # centerXY # pad 1.1

main = mainWith cubicSplineEx
