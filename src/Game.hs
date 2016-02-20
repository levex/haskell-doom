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
import           Enemy
import           Types
import           Var

newtype GameMonad e a = GameMonad { unGame :: ReaderT e IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader e)

data GameState = GameState {
        progId   :: GLuint
      , wad      :: WAD.Wad
      , sideDefs :: Int
      , levelRd  :: RenderData
      , sprites  :: [Sprite]
      , currentSector :: IORef Sector
      , rot      :: IORef GLfloat
      , player   :: IORef Pos
      , enemies  :: IORef [Enemy]
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

-- TODO: put these in another file
type Vertex2D = V2 GLfloat

data Sector = Sector {
      sectorFloorPoints    :: [Vertex2D]
    , sectorWalls          :: [Wall]
} deriving Show

data Wall = Wall {
      wallStart :: Vertex2D
    , wallEnd   :: Vertex2D
    , sector    :: Sector
    , portalTo  :: Maybe Sector
}

instance Show Wall where
    show _ = "I'm a wall"

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

gameLogic :: Game ()
gameLogic = do
  updateSector
  enemiesLogic


enemiesLogic :: Game ()
enemiesLogic =
  enemies $~ map (acquireTarget Thing . moveEnemy)


updateSector :: Game ()
updateSector = return ()



