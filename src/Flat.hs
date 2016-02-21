{-# LANGUAGE OverloadedStrings #-}
module Flat (
  loadFlat,
  flatWidth,
  flatHeight
  ) where

import qualified Game.Waddle          as WAD
import qualified Data.ByteString      as BS
import qualified Data.Map             as M
import           Data.CaseInsensitive
import           Data.Maybe
import           Control.Monad.Reader
import           Data.Word
import           Graphics.GL.Functions
import           Graphics.GL.Core33
import           TextureLoader
import           Game

(flatWidth, flatHeight) = (64, 64)

loadFlatData :: WAD.Wad -> WAD.LumpName -> [Word8]
-- special, transparent flat
loadFlatData :: WAD.Wad -> WAD.LumpName -> [Word8]
loadFlatData wad name
  = BS.unpack $ WAD.flatData flat
  where
    flat = fromMaybe (error "invalid flat") $
             M.lookup (mk name) (WAD.wadFlats wad)

loadFlat :: WAD.LumpName -> Game [GLfloat]
loadFlat name = do
  palette' <- asks palette
  wad'     <- asks wad
  return $ unpackTuples
          (textureDataToColor palette'
            (loadFlatData wad' name))
