-- Needed for `diagrams`
{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

import Data.List
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.BoundingBox

-- ghc --make knot.hs; ./knot -o knot.svg -h 500; chrome knot.svg

-- unused
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

-----------

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
spot = circle 0.1 # fc blue # lw none

-- connect the dots. takes Points (not coords)
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

tanglePoints = TangleCorners { nw = (-1, 1), ne = (1, 0.5),
                             sw = (-1, -0.5), se = (1, -1),
                             midX = 0, midY = 0 }

average xs = sum xs / genericLength xs
polyDelta = 0.2 -- TODO

-- assuming it doesn't use sw and se          
-- TODO abstract out w/ bottomPts
topPts tangle =
       let (nw', ne') = (nw tangle, ne tangle) in
       -- the points may be at diff y-level, so choose the higher y as
       -- a baseline for the padding points' ys
       let bottomY = max (snd nw') (snd ne') in
       let height = average [abs $ fst nw', abs $ fst ne'] / 2 in
       let midpt = (midX tangle, bottomY + height) in
                 -- polyDelta vs. height?
       let leftpad = (fst nw' - polyDelta, bottomY + polyDelta) in
       let rightpad = (fst ne' + polyDelta, bottomY + polyDelta) in
       -- padding points currently unused
       -- map p2 [nw', leftpad, midpt, rightpad, ne']
       map p2 [nw', midpt, ne']

bottomPts tangle =
       -- let tangleFlipY = tangle in
       -- map flipY $ topPts tangleFlipY
       let (sw', se') = (sw tangle, se tangle) in
       let bottomY = min (snd sw') (snd se') in
       let height = average [abs $ fst sw', abs $ fst se'] / 2 in
       let midpt = (midX tangle, bottomY - height {- todo -} ) in
       let leftpad = (fst sw' - polyDelta, bottomY - polyDelta) in
       let rightpad = (fst se' + polyDelta, bottomY - polyDelta) in
       -- map p2 [sw', leftpad, midpt, rightpad, se']
       map p2 [sw', midpt, se']

-- Usage: main = mkPolyhedron1 tanglePoints
-- maybe don't render the diagram, but pass the list of segments?
mkPolyhedron1 :: (Diagram B, TangleCorners) -> Diagram B
mkPolyhedron1 (tangle, tanglePts) = 
              let (topRes, botRes) = (topPts tanglePts, bottomPts tanglePts) in
              tangle
              <> mkSpline False (init topRes)
              <> mkSpline False (tail topRes)
              <> mkSpline False (init botRes)
              <> mkSpline False (tail botRes)
              -- TODO: put curved spline back; this is a hack
              -- <> mkSpline False (topPts tanglePts)
              -- <> mkSpline False (bottomPts tanglePts)

---

-- Draw the 1 crossing (not -1)

-- node :: Int -> Diagram B
-- node n = text (show n) # fontSizeL 0.2 # fc white
--       <> circle 0.1 # fc green # named n

-- crossing :: Int -> Diagram B
-- crossing n = atPoints (trailVertices $ regPoly n 1) (map node [1..n])
--   # connectOutside (1 :: Int) (3 :: Int) # connectOutside (2 :: Int) (4 :: Int)

r45 = rotateBy (1/8)
r90 = rotateBy (1/4)
r180 = rotateBy (1/2)
gap' = 0.35

-- lol sorry
-- crossing length
clen = sqrt 2 / 2

tPts t = [nw t, ne t, sw t, se t]

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- box :: Colour a -> TangleCorners -> Diagram B
box color tc = let spot txt = text txt # fontSizeL 0.1 # fc white
                              <> circle 0.1 # fc color # lw none in
                 let pts = map p2 (tPts tc) in -- TODO: midX, midY
                 position (zip pts [spot "nw", spot "ne", spot "sw", spot "se"])

-- TODO: return the 7 relevant points (overleft, overmid, overright, 
-- underleft, underleft', underright', underright)
-- or 3 relevant segments
overcross :: (Diagram B, TangleCorners)
overcross = let right = unitX in
           let top = (1 - gap') *^ r90 unitX in
           let left = r180 right in
           let bt = r180 top in
           let (gap_b, gap_t) = (r2 (0, -gap'), r2 (0, gap')) in
               -- TODO factor out repeated parts
           let left' = lineFromOffsets [left] # strokeLine in
           let right' = lineFromOffsets [right] # strokeLine in
           let bt' = lineFromOffsets [bt] # strokeLine # translate gap_b in
           let top' = lineFromOffsets [top] # strokeLine # translate gap_t in
           -- let diagram = left' <> right' <> bt' <> top' in
           let diagram = mconcat $ map (translate (r2 (clen, 0)) . r45) [left', right', bt', top'] in
           let coords = TangleCorners -- TODO: remove reliance on coords
               -- top -> nw, left -> ne, left -> sw, bot -> se
                        { nw = (0, clen), ne = (2 * clen, clen),
                          sw = (0, -clen), se = (2 * clen, -clen),
                          midX = clen, midY = 0 } in
          (diagram, coords)

undercross :: (Diagram B, TangleCorners)
undercross = (fst overcross # rotateBy (1/4), snd overcross)

twistSqH :: (Diagram B, TangleCorners) -> Int -> (Diagram B, TangleCorners)
twistSqH crossing n = let width' = clen * fromIntegral n in
            let newCoords = TangleCorners
                        { nw = (0, clen), ne = (2 * width', clen),
                          sw = (0, -clen), se = (2 * width', -clen),
                          midX = width', midY = 0 } in
         -- TODO: remove reliance on coords
         -- confusing: coords could be relative, then whole diagram translated
         (hcat $ replicate n $ fst crossing, newCoords)

-- midX' t = (fst $ nw t + fst $ ne t) / 2
-- midY' t = (snd $ nw t + snd $ sw t) / 2
-- meant to be used as [p2 `diff` p1], where 2 is new and 1 is original
pdiff p2 p1 = (fst p2 - fst p1, snd p2 - snd p1)

mapCorners :: (P2 Double -> P2 Double) -> TangleCorners -> TangleCorners
mapCorners transform' tc =
           let transP p = unp2 $ transform' $ p2 p in
           let (nw', ne') = (transP $ nw tc, transP $ ne tc) in
           let (sw', se') = (transP $ sw tc, transP $ se tc) in
           TangleCorners {
             nw = nw', ne = ne',
             sw = sw', se = se',
           -- by default -- may not be correct for non-rect corners
             midX = (fst nw' + fst ne') / 2,
             midY = (snd nw' + snd sw') / 2 }

-- assuming that d2 is always rectangular; d1 might not be
tangleAdd :: (Diagram B, TangleCorners) -> (Diagram B, TangleCorners)
          -> (Diagram B, TangleCorners)
tangleAdd (d1, c1) (d2, c2) =
          -- position 2nd tangle on right-middle of 1st
          let d1_midY = (snd (nw c1) + snd (sw c1)) / 2 in
          let spc = 2 in -- TODO this should be proportionate to something
          -- TODO: calculate X such that we can connect in straight line
          let d2center = (midX c2, midY c2) in -- TODO or use midX' etc
          let d2halfwidth = (fst (ne c2) - fst (nw c2)) / 2 in
          let d2center' = (fst (ne c1) + d2halfwidth + spc, d1_midY) in
          let d2vector = r2 $ pdiff d2center' d2center in
          let rightAndCenter = translate d2vector in
          let rightAndCenter' = translate d2vector in -- TODO fix type inference
          let (d2', c2') = (rightAndCenter' d2, mapCorners rightAndCenter c2) in
          -- let (d2', c2') = (rightAndCenter d2, mapCorners rightAndCenter c2) in
          let coordsCombine = TangleCorners -- combine from the 2 diagrams
                            { nw = nw c1, ne = ne c2', 
                              sw = sw c1, se = se c2',
                              midX = (fst (nw c1) + fst (ne c2')) / 2,
                              -- note use of d1's midY
                              midY = midY c1 } in -- TODO
          -- TODO: calculate middle/handle point
          let topAdd = mkSpline False (map p2 [ne c1, nw c2']) in
          let bottomAdd = mkSpline False (map p2 [se c1, sw c2']) in
          (d1 <> d2'
          -- <> box green c1
          -- <> box red c2'
          -- <> box blue coordsCombine
          <> topAdd <> bottomAdd
          , coordsCombine)

tangleMult :: (Diagram B, TangleCorners) -> (Diagram B, TangleCorners)
          -> (Diagram B, TangleCorners)
tangleMult (d1, coords1) (d2, coords2) =
           let transform' = reflectY . rotateBy (1/4) in
           let d1' = transform' d1 in
           -- TODO pass Points around
           let transP p = unp2 $ transform' $ p2 p in
           let (nw', ne') = (transP (nw coords1), transP (ne coords1)) in
           let (sw', se') = (transP (sw coords1), transP (se coords1)) in
           let coords1' = TangleCorners
           -- points are transformed and their locations have changed WRT compass
                        { nw = nw', ne = sw',
                          sw = ne', se = se',
                          midX = (fst nw' + fst ne') / 2, -- TODO
                          midY = (fst nw' + fst sw') / 2 } in -- TODO
           tangleAdd (d1', coords1') (d2, coords2)

-- TODO: code to draw just tangle, flip / rotate tangle

-- note to self: natural foldl!
drawTangleN :: [Int] -> Diagram B
drawTangleN [] = mempty
drawTangleN tangle@(t:ts) =             
            let twistAndMultiply acc n = tangleMult acc (twistSqH overcross n) in
            let tangleFinal = foldl twistAndMultiply (twistSqH overcross t) ts in
            let name = intersperse ' ' $ concatMap show tangle in
            let knot = mkPolyhedron1 tangleFinal # lw 2.65 # pad 1.4 in
            -- align text with bottom middle of diagram
            -- doesn't actually work, but leaving code in case I figure it out
            let bbox = boundingBox knot in
            let textp = case getCorners bbox of
                      Nothing -> p2 (0, 0)
                      Just (bt_lt, top_rt) ->
                           let ((bx, by), (tx, ty)) = (unp2 bt_lt, unp2 top_rt) in
                           p2 (average [bx, tx], by) 
            in
            let kcenter = case boxCenter bbox of
                            Nothing -> p2 (0, 0)
                            Just c -> c in
            -- let text_diag = text name # fontSizeL 2
            --      # font "freeserif" # pad 1.2 # centerX in
            -- position [(textp, text_diag), (kcenter, knot)]
            (alignedText 1 1 name # fontSizeL 1.2
                 # font "freeserif" # pad 1.2 # centerX)
            === knot
            -- # pad 1.2

--------------

partitions :: Int -> [[Int]]
partitions n
  | n <= 0 = [[]]
  | otherwise =
  let choose n = [1, 2 .. n] in
  let subpartition y = map (y :) $ partitions (n - y) in
  concatMap subpartition (choose n)

partitionsUpTo :: Int -> [(Int, [[Int]])]
partitionsUpTo n
 | n <= 0 = []
 | otherwise = map (\x -> (x, partitions x)) [1..n]


rawKnotsTo7Crossings :: Diagram B
rawKnotsTo7Crossings =
   let notations = partitionsUpTo 4 in
   -- TODO break lines into fives and put "knots of n crossings" text
   -- let drawn = map (\(crossings, tangles) -> (crossings, map drawTangleN tangles))
   --             notations in
   let drawn' = map (\(crossings, tangles) -> hcat $ map drawTangleN tangles)
               notations in
   vcat drawn'

--------------

testDiagram :: Diagram B
testDiagram =
     (drawTangleN [1]
     ||| drawTangleN [2]
     ||| drawTangleN [3])
     ===
     (drawTangleN [1, 1]
     ||| drawTangleN [2, 1]
     ||| drawTangleN [2, 3])
     ===
     (drawTangleN [3, 2]
     ||| drawTangleN [1, 4]
     ||| drawTangleN [4, 1])
     ===
     (drawTangleN [1, 1, 1]
     ||| drawTangleN [2, 1, 3]
     ||| drawTangleN [6, 6, 6])
     ===
     (drawTangleN [2, 1, 1, 1, 2]
     -- ||| drawTangleN [6, 6, 6, 6, 6, 6]
     ||| drawTangleN [1, 2, 3, 4, 5])

   
-- TODO monadically pass around coords
-- TODO display tangle numbers, enumerate all
-- TODO confirm that this is right
-- TODO crossings rendered badly when img too large
-- TODO fix forking at intersections
-- TODO get rid of whitespace and use proportional LW
main = mainWith $ rawKnotsTo7Crossings # centerXY # pad 1.01
