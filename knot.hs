{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

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

pts = map p2 [(0,0), (2,3), (5,-2), (-4,1), (0,3)]
spot = circle 0.2 # fc blue # lw none
mkPath closed = position (zip pts (repeat spot))
             <> cubicSpline closed pts

cubicSplineEx :: Diagram B
cubicSplineEx = (mkPath False ||| strutX 2 ||| mkPath True)
              # centerXY # pad 1.1

main = mainWith cubicSplineEx
