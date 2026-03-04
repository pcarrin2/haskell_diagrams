> {-# LANGUAGE NoMonomorphismRestriction #-}
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import System.Random
> import Data.Colour.RGBSpace.HSL
> import Data.Colour.SRGB
> import System.IO

This algorithm recursively transforms and combines equilateral triangles into a final equilateral-triangle-shaped figure.

First we define the base case, which is an equilateral triangle one unit high, with the triangle's origin at the midpoint of its base.

> baseTri :: Diagram B
> baseTri = triangle (2 / sqrt 3) # alignB # lw none

Next we define some transformations on diagrams that have the same outline as baseTri.

The combineTris function combines two input diagrams shaped like baseTri into a result shaped like baseTri. It does this by shearing and scaling both triangles, then reflecting the second triangle, as shown.

> combineTris :: Double -> Diagram B -> Diagram B -> Diagram B
> combineTris proportion diagram1 diagram2 = 
>   -- first diagram becomes bottom triangle
>   (
>     diagram1 # 
>    shearX (proportion / sqrt 3) # 
>    scaleToY (1 - proportion)
>    ) 
>  -- second diagram becomes top triangle
>  <> 
>  (
>    diagram2 # 
>    shearX ((1 - proportion) / sqrt 3) # 
>    scaleToY proportion # 
>    reflectAbout (p2 ((-1) / sqrt 3, 0)) (rotate (pi/6 @@ rad) xDir)
>    )

The triRotate function just rotates a triangle-shaped diagram 120 degrees and then puts the origin back in the right place.

> triRotate :: Diagram B -> Diagram B
> triRotate diagram = diagram # translateY (-1/3) # rotateBy (1/3) # alignB

The triRotateN function performs triRotate a given number of times.

> triRotateN :: Int -> Diagram B -> Diagram B
> triRotateN n diagram = iterate triRotate diagram !! n

This helper function takes an infinite list and lazily splits it into even-indexed and odd-indexed lists. Since each step of our recursive function will call itself twice, this is a handy way to pass a list of random values to each child.

> lazySplit :: [a] -> ([a], [a])
> lazySplit xs = (evens xs, odds xs)
>   where
>     evens :: [a] -> [a]
>     evens []  = []
>     evens [x] = [x]
>     evens (x:_:xs) = x : evens xs
>     odds :: [a] -> [a]
>     odds []  = []
>     odds [x] = []
>     odds (_:x:xs) = x : odds xs

And because we want our final result to look nice, here's a helper function to generate a pretty color, given a hue.

> prettyColor :: Double -> Colour Double
> prettyColor hue = 
>   let rgbTuple = hsl (hue*360) 0.8 0.7
>   in sRGB (channelRed rgbTuple) (channelGreen rgbTuple) (channelBlue rgbTuple)

Here is the recursive function. We give this function a number of steps, an infinite list of random doubles, and an infinite list of random integers. In the base case, it just returns baseTri filled with a pretty color. Otherwise, it calls itself twice to generate two sub-figures, then combines them with combineTris, and finally rotates the combined figure a random number of times.

> triRecurse :: Int -> [Double] -> [Int] -> Diagram B
> triRecurse steps randdoubles randints
>   | steps <= 0 = baseTri # fc (prettyColor d)
>   | otherwise  = combineTris d (triRecurse (steps - 1) fstDs fstRs) (triRecurse (steps - 1) sndDs sndRs) # triRotateN r
>   where 
>     r = head randints
>     (fstRs, sndRs) = lazySplit $ tail randints
>     d = head randdoubles
>     (fstDs, sndDs) = lazySplit $ tail randdoubles

In the main function, we generate random seeds for the lists of integers and doubles.

> main :: IO ()
> main = do
>   randIntsSeed <- randomIO :: IO Int
>   randDoublesSeed <- randomIO :: IO Int
>   mainWith $ triRecurse 12 (randoms $ mkStdGen randDoublesSeed) (randomRs (0, 2) $ mkStdGen randIntsSeed)
