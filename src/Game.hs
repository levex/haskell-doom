{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game where
import Control.Monad.IO.Class
import Control.Monad.Reader
-- ugly
import qualified Game.Waddle          as WAD
import           Graphics.GL.Core33
import           Graphics.UI.GLFW
import           Linear
import           Data.IORef

newtype GameMonad e a = GameMonad { unGame :: ReaderT e IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader e)

data GameState = GameState {
        progId   :: GLuint,
        wad      :: WAD.Wad,
        sideDefs :: Int,
        rot      :: IORef GLfloat,
        player   :: IORef (V4 GLfloat)
    }

type Game a = GameMonad GameState a

runGame :: GameMonad e a -> e -> IO a
runGame = runReaderT . unGame

io :: IO a -> GameMonad e a
io = liftIO
