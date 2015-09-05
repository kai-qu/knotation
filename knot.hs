{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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
cubicSplineEx = (twist 5 (0, 0) ||| (twist 3 (5, 5) # rotateBy (1/4)))
                # centerXY # pad 1.1

main = mainWith cubicSplineEx
