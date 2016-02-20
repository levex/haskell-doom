{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Game where
import Control.Monad.IO.Class
import Control.Monad.Reader
import Foreign.Ptr
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
        levelRd  :: RenderData,
        sprites  :: [Sprite],
        player   :: IORef (V4 GLfloat)
    }

data RenderData = RenderData {
      rdVbo  :: GLuint
    , rdEbo  :: GLuint
    , rdVao  :: GLuint
    , rdTex  :: GLuint
    , rdProg :: GLuint
}

data Sprite = Sprite {
        spriteName       :: String,     -- sprite name in WAD
        spriteActive     :: IORef Bool, -- whether we can start moving
        spriteAnimFrame  :: IORef Int,  -- current animation frame
        spriteRenderData :: RenderData
    }

bindRenderData :: MonadIO m => RenderData -> m ()
bindRenderData rd = do
  glUseProgram (rdProg rd)
  glBindVertexArray (rdVao rd)
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER (rdEbo rd) 
  glBindTexture GL_TEXTURE_2D (rdTex rd)

type Game a = GameMonad GameState a

runGame :: GameMonad e a -> e -> IO a
runGame = runReaderT . unGame

io :: IO a -> GameMonad e a
io = liftIO
