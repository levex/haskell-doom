{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Game where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.IORef
import Data.Var
import Enemy
import Graphics.GL.Core33
import Graphics.Program
import Level.Sector
import Render
import Sprite
import TextureLoader
import Types

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
