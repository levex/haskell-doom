{-# LANGUAGE OverloadedStrings #-}
module Sprite where
-- TODO: prune
import           Control.Monad
import           Control.Monad.Reader
import           Data.CaseInsensitive hiding (map)
import           Data.Foldable
import           Data.IORef
import           Data.Array.IO        as AI
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.Map             as M
import           Data.Maybe
import           Foreign
import           Foreign.C.String
import           Game
import qualified Game.Waddle          as WAD
import           GLUtils
import           Graphics.GL.Core33
import           Graphics.UI.GLFW
import           Linear
import           Var
import           Window
import           TextureLoader
import           Data.Array.IO
import Data.List

testSpriteVbo :: Float -> Float -> [GLfloat]
testSpriteVbo k n = [
  -1/8,  1, 0.0,   k/n, 0.0,
   1/8,  1, 0.0,   (k+1)/n, 0.0,
  -1/8, -1, 0.0,   k/n, 1,
   1/8, -1, 0.0,   (k+1)/n, 1]

testSpriteEbo :: [GLuint]
testSpriteEbo = [
  0, 1, 2,
  2, 1, 3]

loadSprite :: [WAD.Sprite] -> IO [Word8]
loadSprite sprites@(s:_) = do
  let pic = WAD.spritePicture s
  let fW = WAD.pictureWidth pic
  let fH = WAD.pictureHeight pic
  pxArr <- AI.newArray (0, fW * fH * length sprites) (0xFF :: Word8)
            :: IO (IOArray Int Word8)
  forM_ (zip [0..] sprites) $ \(n, col) -> do
    let posts = WAD.picturePosts pic
    forM_ (zip [0..] posts) $ \(x, col) ->
      forM_ col $ \post -> do
        let tx = x
        forM_ (zip [0..] (BS.unpack $ WAD.postPixels post)) $ \(i, pt) -> do
          let ty = (fromIntegral $ WAD.postTop post) + i
          writeArray pxArr (tx + ty * fW + fW * n)  pt
  getElems pxArr

makeSprite :: WAD.Wad -> GLuint -> WAD.LumpName -> IO Sprite
makeSprite wad progId spriteName = do
  vaoId <- withNewPtr (glGenVertexArrays 1)
  glBindVertexArray vaoId

  vboId <- withNewPtr (glGenBuffers 1)
  glBindBuffer GL_ARRAY_BUFFER vboId
  -- needs unhardcoding
  withArrayLen (testSpriteVbo 0 8) $ \len vertices ->
      glBufferData GL_ARRAY_BUFFER
                    (fromIntegral $ len * sizeOf (0 :: GLfloat))
                    (vertices :: Ptr GLfloat)
                    GL_STATIC_DRAW

  eboId <- withNewPtr (glGenBuffers 1)
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER eboId
  withArrayLen testSpriteEbo $ \len vertices ->
      glBufferData GL_ELEMENT_ARRAY_BUFFER
                    (fromIntegral $ len * sizeOf (0 :: GLuint))
                    (vertices :: Ptr GLuint)
                    GL_STATIC_DRAW

  -- load sprite image
  let spriteMap = WAD.wadSprites wad
  let sprites@(sprite:_) = catMaybes [M.lookup (mk $ BS.concat [spriteName, BSC.singleton c, "1"]) spriteMap | c <- ['A'..'Z']]
  let loadedPalette = loadPalettes wad
  p <- loadSprite sprites
  let spritePixels = unpackTuples (textureDataToColor loadedPalette p)
  let sW = fromIntegral $ WAD.pictureWidth $ WAD.spritePicture sprite
  let sH = fromIntegral $ WAD.pictureHeight $ WAD.spritePicture sprite
      n = length sprites
  texId <- withNewPtr (glGenTextures 1)
  glBindTexture GL_TEXTURE_2D texId

  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
  withArray spritePixels $
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) (sW * fromIntegral n) sH 0 GL_RGBA GL_FLOAT

  posAttrib <- get $ AttribLocation progId "position"
  glEnableVertexAttribArray posAttrib
  glVertexAttribPointer posAttrib
                        3
                        GL_FLOAT
                        (fromBool False)
                        (fromIntegral $ 5 * sizeOf (0 :: GLfloat))
                        nullPtr

  colAttrib <- get $ AttribLocation progId "texcoord"
  glEnableVertexAttribArray colAttrib
  glVertexAttribPointer colAttrib
                        2
                        GL_FLOAT
                        (fromBool False)
                        (fromIntegral $ 5 * sizeOf (0 :: GLfloat))
                        (offsetPtr 3 (0 :: GLfloat))

  let renderData = RenderData { rdVao = vaoId,
                     rdVbo = vboId,
                     rdTex = texId,
                     rdProg = progId,
                     rdEbo = eboId}

  Sprite <$> pure "Lev"
         <*> newIORef False
         <*> newIORef 0
         <*> pure renderData
