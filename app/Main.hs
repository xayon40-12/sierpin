module Main where
    
import Graphics.Gloss
import System.Environment

width = 800 :: Int
hight = 800 :: Int

main :: IO ()
main = do
    args <- getArgs
    let r = if not $ null args then read $ head args else 0.5
    animate (InWindow "sierpin" (width, hight) (0, 0)) white (draw r)

draw :: Float -> Float -> Picture
draw r t = ccircle 0 0 100 t r 3

ccircle :: Float -> Float -> Float -> Float -> Float -> Float -> Picture
ccircle x y d t r n = pictures $ Translate x y (color blue $ circle d):[ccircle (x+(d+t)*cos (a+t*r)) (y+(d+t)*sin (a+t*r)) (d/2) (t*1.1) r n | d>1, a <- map (*(2*pi/n)) [0..n-1]]
