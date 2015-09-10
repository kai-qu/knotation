-- Needed for `diagrams`
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

import Data.List
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- ghc --make knot.hs; ./knot -o knot.svg -h 500; chrome knot.svg

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
-- doesn't account for tangle addition

-- Pos = left-to-right, bottom strand goes to the top
data Direction' = Neg | Pos deriving (Show, Eq)

data Tangle =
     Zero | Infinity | Twist Direction' Int
     | Add Tangle Tangle | Mult Tangle Tangle
     -- what about converting Mult to reflect, rotate, and add??
     -- can't do reflect and rotate because we don't handle coords here?
     deriving (Show, Eq)

-- more low-level
-- overstrand (middle, start and end of 1 segment)
-- understand (start and end of 2 segments)
-- at 45 degrees
-- translate a tangle into over/under crossings with coordinates? or just rela
-- tive positions?
data Crossing = Under | Over
-- data Crossing' = Crossing' {
--      mid :: Pt
--      , over :: (Pt, Pt

{-
data TangleCorners = TangleCorners { -- northwest, northeast, etc. + location of mid
     nw :: Pt
     , ne :: Pt
     , sw :: Pt
     , se :: Pt
     , midX :: Double
     , midY :: Double -- delete this or use bounding box instead?
     } deriving (Show)
-}
-- compile 1 -> Twist Pos 1 -> Crossing ? -> ? + Polyhedra

-- ? fix this. data Knot = ... polyhedra, locations..

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
twistPts (baseX, baseY) = 
  let w' = w / 2.0 in
  let h' = h / 4.0 in
  map p2 [(baseX - offCenter, baseY), (baseX - w', baseY - h'),
          (baseX + w', baseY - (h' * 3)), (baseX + offCenter, baseY - h)]
spot = circle 0.02 # fc blue # lw none

-- connect the dots
mkSpline showPts points =
         if showPts then
         position (zip points (repeat spot)) <> cubicSpline False points
         else cubicSpline False points -- spline not closed
mkTwist base = mkSpline True (twistPts base)

twistOffsetX = 0
delta = 0.2
twistOffsetY = h / 2.0 + delta
twistOffsetP n = (twistOffsetX, n * twistOffsetY)

smash xs = foldl (<>) mempty xs

-- twist :: Diagram B
twist n base =
      let translateBy n obj = obj # translate (r2 $ twistOffsetP n) in
      let twists = take n $ zipWith translateBy [0,1..] (repeat $ mkTwist base) in
      let halfTwists = [mempty] in -- [topHalf 0, bottomHalf n] -- TODO
      smash $ twists ++ halfTwists
      -- hcat [mkTwist False, mkTwist False]
      -- mkTwist False ||| mkTwist False

cubicSplineEx :: Diagram B
cubicSplineEx = twist 3 (0, 0) # rotateBy (1/4)
              -- TODO: for multiple twists, endpoints + auto-placement +
              -- the no-rotate method?
              -- for figure-8      
              -- (twist 2 (0, 0) ||| (twist 2 (0, 0) # rotateBy (1/4)))
                # centerXY # pad 1.1

-------

-- Draw the 1* polyhedron

type Pt = (Double, Double)
data TangleCorners = TangleCorners { -- northwest, northeast, etc. + location of mid
     nw :: Pt
     , ne :: Pt
     , sw :: Pt
     , se :: Pt
     , midX :: Double
     , midY :: Double -- delete this or use bounding box instead?
     } deriving (Show)

tPts t = [nw t, ne t, sw t, se t]

testTangle = TangleCorners { nw = (-1, 1), ne = (1, 0.5),
                             sw = (-1, -0.5), se = (1, -1),
                             midX = 0, midY = 0 }

average xs = sum xs / genericLength xs
polyDelta = 0.5 -- TODO

-- assuming it doesn't use sw and se          
topPts tangle =
       let (nw', ne') = (nw tangle, ne tangle) in
       -- the points may be at diff y-level, so choose the higher y as
       -- a baseline for the padding points' ys
       let bottomY = max (snd nw') (snd ne') in
       let height = average [abs $ fst nw', abs $ fst ne'] in
       let midpt = (midX tangle, bottomY + height) in
                 -- polyDelta vs. height?
       let leftpad = (fst nw' - polyDelta, bottomY + polyDelta) in
       let rightpad = (fst ne' + polyDelta, bottomY + polyDelta) in
       map p2 [nw', leftpad, midpt, rightpad, ne']

bottomPts tangle =
       -- let tangleFlipY = tangle in
       -- map flipY $ topPts tangleFlipY
       let (sw', se') = (sw tangle, se tangle) in
       let bottomY = min (snd sw') (snd se') in
       let height = average [abs $ fst sw', abs $ fst se'] in
       let midpt = (midX tangle, bottomY - height {- todo -} ) in
       let leftpad = (fst sw' - polyDelta, bottomY - polyDelta) in
       let rightpad = (fst se' + polyDelta, bottomY - polyDelta) in
       map p2 [sw', leftpad, midpt, rightpad, se']

-- Usage: main = mkPolyhedron1 testTangle
mkPolyhedron1 :: TangleCorners -> Diagram B
mkPolyhedron1 tangle =
              mkSpline False (topPts tangle) <> mkSpline False (bottomPts tangle)

---

-- Draw the 1 crossing (not -1)

-- node :: Int -> Diagram B
-- node n = text (show n) # fontSizeL 0.2 # fc white
--       <> circle 0.1 # fc green # named n

-- crossing :: Int -> Diagram B
-- crossing n = atPoints (trailVertices $ regPoly n 1) (map node [1..n])
--   # connectOutside (1 :: Int) (3 :: Int) # connectOutside (2 :: Int) (4 :: Int)

r90 = rotateBy (1/4)
r180 = rotateBy (1/2)
gap' = 0.2

-- TODO: return the 7 relevant points (overleft, overmid, overright, 
-- underleft, underleft', underright', underright)
-- or 3 relevant segments
overcross :: Diagram B
overcross = let right = unitX in
           let top = (1 - gap') *^ r90 unitX in
           let left = r180 right in
           let bottom = r180 top in
           let (gap_b, gap_t) = (r2 (0, -gap'), r2 (0, gap')) in
           fromOffsets [left]
           <> fromOffsets [right]
           <> lineFromOffsets [bottom] # strokeLine # translate gap_b
           <> lineFromOffsets [top] # strokeLine # translate gap_t

undercross :: Diagram B
undercross = overcross # rotateBy (1/4)

main = mainWith $
       overcross ||| undercross
       # centerXY # pad 1.1
