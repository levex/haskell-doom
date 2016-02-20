{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module TextureLoader
    ( loadTexture
    , loadPalettes
    , ColorPalette
    , unpackTuples
    , textureDataToColor
    , getColor
    )
    where

import           Control.Monad
import           Control.Monad.Reader
import           Data.Array.IO           as AI
import qualified Data.ByteString         as BS
import           Data.CaseInsensitive
import           Data.Map.Lazy           as Map
import           Data.Maybe
import           Data.Word
import qualified Game.Waddle             as WAD
import           Graphics.GL.Core33
import           Graphics.GL.Functions
import           Game

type ColorPalette = [[(Word8, Word8, Word8)]]

loadPalettes :: WAD.Wad -> ColorPalette
loadPalettes wad
  | isNothing pals = []
  | otherwise      = p
  where
    pals@(~(Just (WAD.Palettes p))) = WAD.wadPalettes wad

textureDataToColor :: ColorPalette -> [Word8] -> [(GLfloat, GLfloat, GLfloat, GLfloat)]
textureDataToColor palette words
  = (\i -> getColor (fromIntegral i) palette) <$> words

getColor :: Int -> ColorPalette -> (GLfloat, GLfloat, GLfloat, GLfloat)
getColor 0xFF cp 
  = (0.0, 0.0, 0.0, 0.0)
getColor n cp
  = (\(r, g, b) -> (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255, 1.0))
          . (!! n) . head $ cp

unpackTuples :: [(a, a, a, a)] -> [a]
unpackTuples = concatMap (\(r, g, b, a) -> [r, g, b, a])

loadTexture :: WAD.LumpName -> Game (GLsizei, GLsizei, [GLfloat])
loadTexture name = do
  wad' <- asks wad
  let myTex = fromJust (Map.lookup (mk name) (WAD.wadTextures wad'))
  let texWidth = fromIntegral $ WAD.textureWidth myTex
  let texHeight = fromIntegral $ WAD.textureHeight myTex
  let loadedPalette = loadPalettes wad'
  pxArr <- liftIO (AI.newArray (0, texWidth*texHeight) (0xFF :: Word8)
            :: IO (IOArray Int Word8))
  forM_ (WAD.texturePatchDescriptors myTex) $ \desc -> do
    let bx = fromIntegral $ WAD.patchDescriptorXOffset desc
        by = fromIntegral $ WAD.patchDescriptorYOffset desc
        idx = fromIntegral $ WAD.patchDescriptorPNameIndex desc
        lname = fromJust $ Map.lookup idx (WAD.wadPNames wad')
        patch = fromJust $ Map.lookup (mk lname) (WAD.wadPatches wad')
    let posts = WAD.picturePosts $ WAD.patchPicture patch
    forM_ (zip [0..] posts) $ \(x, col) ->
      forM_ col $ \post -> do
        let tx = bx + x
        forM_ (zip [0..] (BS.unpack $ WAD.postPixels post)) $ \(i, pt) -> do
          let ty = by + fromIntegral (WAD.postTop post) + i
          when (tx <= texWidth - 1 && ty <= texHeight - 1 && tx >= 0 && ty >= 0) $
            liftIO (writeArray pxArr (tx + ty * texWidth) pt)
  final <- liftIO $ getElems pxArr
  return (fromIntegral texWidth,
          fromIntegral texHeight,
          unpackTuples (textureDataToColor loadedPalette final))
