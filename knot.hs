import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Direction = Horiz | Vert
data Mode = Make | Delete | Bind | Rotate
data Crossing = Crossing { id :: Int
                         , dir :: Direction
                         , pt :: Point
                         }
data World = World { mode :: Mode
                   , state :: [Crossing]
                   }

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
        handler
        (\t -> \w -> w) -- no timestep

initWorld :: [Picture]
initWorld = let thickness = 20 in
          let twist1 = map (rotate 70) [Arc 0 160 100, Arc 0 160 (100 + thickness)] in
          let twist2 = map (translate 0 (200 + thickness) . rotate 70) [Arc 180 340 100, Arc 180 340 (100 + thickness)] in
          let twist = twist1 ++ twist2 in
          let x = 0.0 in
          let y = 240.0 in
          let init = 500.0 in
          let twistall = concatMap (\(y, t) -> map (translate x y) t) $ zip [init, (init - y) ..] (replicate 5 twist) in
          map (translate 0 (-80) . scale 0.5 0.5) twistall
          -- twist ++ map (translate 0 (-100)) twist
-- initWorld = [crossing Vert (0, 0) 30 80,
             -- crossing Horiz (100, 100) 50 150]

handler :: Event -> [Picture] -> [Picture]
handler event world = case event of
                        EventKey (MouseButton LeftButton) Up _ (x, y) ->
                          let newC = crossing' Vert (x, y) in
                          (newC : world)
                        _ -> world

crossing' dir (x, y) = crossing dir (x, y) 30 80

-- positive floats
-- TODO: make crossing a type, with this being the picture function for it
-- make world state a type
-- UI design: "modes" for new, delete, bind, rotate?
-- TODO: can't move crossing onto another
crossing :: Direction -> Point -> Float -> Float -> Picture
crossing dir (x, y) thickness length =
         let lenhalf = length / 2 in
         let thickhalf = thickness / 2 in
         let linewidth = 1 in
         let left = line [(-thickhalf, -lenhalf), (-thickhalf, lenhalf)] in
         let right = translate thickness 0 left in
         let bottom = line [(-lenhalf, -thickhalf), (lenhalf, -thickhalf)] in
         let top = translate 0 thickness bottom in
         -- assumes lines of thickness 1
         let (horiz, vert) = case dir of
                               Horiz -> (thickness + linewidth,
                                         thickness - linewidth)
                               Vert  -> (thickness - linewidth,
                                         thickness + linewidth) in
         let over = color white $ rectangleSolid horiz vert in
         pictures $ map (translate x y) $ [left, right, bottom, top, over]
