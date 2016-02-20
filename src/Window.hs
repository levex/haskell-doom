module Window(initGL) where
import Graphics.UI.GLFW 
import Prelude hiding (init)
import Control.Monad
import Data.Maybe

type MainLoop = (Window -> IO ()) -> IO ()

initGL :: String -> Int -> Int -> IO MainLoop
initGL windowTitle width height = do
    setErrorCallback (Just simpleErrorCallback)
    r <- init -- from GLFW
    unless r $ error "Error initializing GLFW!"

    windowHint $ WindowHint'ClientAPI ClientAPI'OpenGL
    windowHint $ WindowHint'OpenGLForwardCompat True
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 2

    -- Create a window, and store the context
    m <- createWindow width height windowTitle Nothing Nothing
    let w = fromMaybe (error "Couldn't create window") m

    setWindowPos w 0 0
    makeContextCurrent m

    -- Return a function which should be used when the window is updated
    return (updateWindow w)

updateWindow :: Window -> MainLoop
updateWindow w io
    = windowShouldClose w >>= \q ->
        if q
            then terminate
            else do
                io w
                swapBuffers w
                pollEvents
                updateWindow w io

simpleErrorCallback :: (Show a, Show b) => a -> b -> IO ()
simpleErrorCallback e s
    = putStrLn $ unwords [show e, show s]
