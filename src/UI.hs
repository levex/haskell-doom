{-# LANGUAGE OverloadedStrings #-}
module UI where

import qualified Data.Map             as M
import qualified Data.ByteString      as BS
import qualified Game.Waddle          as WAD
import           Data.Maybe
import           Data.CaseInsensitive hiding (map)
import           Control.Monad.Reader

import Game

uiTest :: Game RenderData
uiTest = do
  wad'     <- asks wad
  palette' <- asks palette

  let lump = fromMaybe (error "UI not found")
                (M.lookup (mk "STBAR") (WAD.wadLumpLookup wad'))
  return undefined
