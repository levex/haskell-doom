module Main where
import           Window

width :: Int
height :: Int
(width, height) = (1280, 1024)

main :: IO ()
main = do
    mainLoop <- initGL "Window name" width height

    mainLoop (const (return ()))
