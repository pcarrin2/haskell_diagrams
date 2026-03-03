{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import System.Random
import Data.Colour.RGBSpace.HSL
import Data.Colour.SRGB
import System.IO

-- equilateral triangle one unit high, with the origin at the midpoint of the base
baseTri :: Diagram B
baseTri = triangle (2 / sqrt 3) # alignB # lw none

-- combines two triangle-shaped input diagrams into a triangle-shaped result
combineTris :: Double -> Diagram B -> Diagram B -> Diagram B
combineTris proportion diagram1 diagram2 = 
  -- first diagram becomes bottom triangle
  (
    diagram1 # 
    shearX (proportion / sqrt 3) # 
    scaleToY (1 - proportion)
    ) 
  -- second diagram becomes top triangle
  <> 
  (
    diagram2 # 
    shearX ((1 - proportion) / sqrt 3) # 
    scaleToY proportion # 
    reflectAbout (p2 ((-1) / sqrt 3, 0)) (rotate (pi/6 @@ rad) xDir)
    )

-- rotates a triangle-shaped input diagram by 1/3 turn aka 120 degrees CCW
triRotate :: Diagram B -> Diagram B
triRotate diagram = diagram # translateY (-1/3) # rotateBy (1/3) # alignB

-- performs the triRotate operation n times
triRotateN :: Int -> Diagram B -> Diagram B
triRotateN n diagram = iterate triRotate diagram !! n

-- uses a recursive zipper operation to lazily split an infinite list into even and odd values
lazySplit :: [a] -> ([a], [a])
lazySplit xs = (evens xs, odds xs)
  where
    evens :: [a] -> [a]
    evens []  = []
    evens [x] = [x]
    evens (x:_:xs) = x : evens xs
    odds :: [a] -> [a]
    odds []  = []
    odds [x] = []
    odds (_:x:xs) = x : odds xs

-- generates a pretty color from a hue
prettyColor :: Double -> Colour Double
prettyColor hue = 
  let rgbTuple = hsl (hue*360) 0.8 0.7
  in sRGB (channelRed rgbTuple) (channelGreen rgbTuple) (channelBlue rgbTuple)

-- recursively combines triangles, starting with the baseTri shape
triRecurse :: Int -> [Double] -> [Int] -> Diagram B
triRecurse steps randdoubles randints
  | steps <= 0 = baseTri # fc (prettyColor d)
  | otherwise  = combineTris d (triRecurse (steps - 1) fstDs fstRs) (triRecurse (steps - 1) sndDs sndRs) # triRotateN r
  where 
    r = head randints
    (fstRs, sndRs) = lazySplit $ tail randints
    d = head randdoubles
    (fstDs, sndDs) = lazySplit $ tail randdoubles


main :: IO ()
main = do
  randIntsSeed <- randomIO :: IO Int
  randDoublesSeed <- randomIO :: IO Int
  mainWith $ triRecurse 10 (randoms $ mkStdGen randDoublesSeed) (randomRs (0, 2) $ mkStdGen randIntsSeed)
