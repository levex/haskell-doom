{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Game where
import Control.Monad.IO.Class
import Control.Monad.Reader
-- ugly
import           Data.Word
import           Level.Sector
import           Graphics.Program
import           Graphics.GL.Core33
import           Render
import           Data.IORef
import           Enemy
import           Types
import           Data.Var

newtype GameMonad e a = GameMonad { unGame :: ReaderT e IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader e)

data GameState u i = GameState {
        prog          :: Program u i
      , sideDefs      :: Int
      , levelRd       :: [RenderData]
      , floorRd       :: RenderData
      , sprites       :: [Sprite]
      , currentSector :: IORef Sector
      , rot           :: IORef GLfloat
      , player        :: IORef Pos
      , enemies       :: IORef [Enemy]
      , palette       :: ColorPalette
      , sky           :: RenderData
      , pWeapon       :: RenderData
      , ticks         :: IORef Int
      , lastShot      :: IORef Int
    }

data Sprite = Sprite {
        spriteName       :: String,     -- sprite name in WAD
        spriteActive     :: IORef Bool, -- whether we can start moving
        spriteAnimFrame  :: IORef Int,  -- current animation frame
        spriteRenderData :: RenderData,
        spritePos        :: Pos
    }

type ColorPalette = [[(Word8, Word8, Word8)]]

type Game a = forall u i. GameMonad (GameState u i) a

runGame :: GameMonad e a -> e -> IO a
runGame = runReaderT . unGame

gameLogic :: Game ()
gameLogic = do
  updateSector
  enemiesLogic


enemiesLogic :: Game ()
enemiesLogic =
  enemies $~ map (acquireTarget Thing . moveEnemy)


updateSector :: Game ()
updateSector = return ()
