{-# LANGUAGE StandaloneDeriving #-}
module Sprite where
-- TODO: prune
import           Control.Monad
import           Control.Monad.Reader
import           Data.CaseInsensitive hiding (map)
import           Data.Foldable
import           Data.IORef
import           Data.Array.IO         as AI
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Char
import           Foreign
import           Foreign.C.String
import           Game
import qualified Game.Waddle           as WAD
import           GLUtils
import           Graphics.GL.Core33
import           Graphics.UI.GLFW
import           Linear
import           Var
import           Window
import           Types
import           TextureLoader
import           SpriteMap
import           Data.Array.IO

testSpriteVbo :: [GLfloat]
testSpriteVbo = [
  -0.5,  0.5, 0.0,   0.0, 0.0,
   0.5,  0.5, 0.0,   1.0, 0.0,
  -0.5, -0.5, 0.0,   0.0, 1.0,
   0.5, -0.5, 0.0,   1.0, 1.0]

testSpriteEbo :: [GLuint]
testSpriteEbo = [
  0, 1, 2,
  2, 1, 3]

loadSpriteColor :: WAD.Sprite -> ColorPalette -> IO [GLfloat]
loadSpriteColor sprite cp
  = unpackTuples <$> (textureDataToColor cp <$> loadSprite sprite)

loadSprite :: WAD.Sprite -> IO [Word8]
loadSprite sprite = do
  let pic = WAD.spritePicture sprite
  let fW = WAD.pictureWidth pic
  let fH = WAD.pictureHeight pic
  pxArr <- AI.newArray (0, fW * fH) (0xFF :: Word8)
            :: IO (IOArray Int Word8)
  let posts = WAD.picturePosts pic
  forM_ (zip [0..] posts) $ \(x, col) ->
    forM_ col $ \post -> do
      let tx = x
      forM_ (zip [0..] (BS.unpack $ WAD.postPixels post)) $ \(i, pt) -> do
        let ty = (fromIntegral $ WAD.postTop post) + i
        when (tx <= fW - 1 && ty <= fH - 1 && tx >= 0 && ty >= 0) $
          writeArray pxArr (tx + ty * fW) pt
  getElems pxArr

createLevelThings :: WAD.Wad -> GLuint -> [WAD.Thing] -> IO [Sprite]
createLevelThings wad progId things
  = mapM (\t -> makeSprite' (mkVbo t) (mkEbo t) (Just t) wad progId (thingToSprite $ WAD.thingType t))
        (filter (\t ->
            notElem (thingTypeToInt $ WAD.thingType t) reservedSpriteIds)
        things)
    where
      pW = 3 -- fixME, ugly
      pH = 3 -- fixME, ugly
      tx t = fromIntegral (WAD.thingX t) / scale
      ty t = fromIntegral (WAD.thingY t) / scale
      mkVbo t = [ - tx t,      pW, ty t,        1, 0
                , - tx t + pH, pW, ty t + pH,   0, 0
                , - tx t,      0,  ty t,        1, 1
                , - tx t + pH, 0,  ty t + pH,   0, 1
                ]
      mkEbo t = [
        0, 1, 2,
        2, 1, 3]

makeSprite :: WAD.Wad -> GLuint -> WAD.LumpName -> IO Sprite
makeSprite
  = makeSprite' testSpriteVbo testSpriteEbo Nothing

findSpriteName :: WAD.Wad -> WAD.LumpName -> WAD.LumpName
findSpriteName wad name
  = findSpriteName' wad name "A" "0"
  where
    findSpriteName' wad name f@(a : as) g@(b : bs)
      | isNothing p = findSpriteName' wad name (na : as) (nb : bs)
      | otherwise   = t
      where
        p = M.lookup (mk t) (WAD.wadSprites wad)
        t = BS.append (BS.append name (BSC.pack f)) (BSC.pack g)
        na = chr ((ord a) + 1)
        nb = chr ((ord b) + 1)

makeSprite' :: [GLfloat] -> [GLuint] -> Maybe WAD.Thing -> WAD.Wad -> GLuint -> WAD.LumpName -> IO Sprite
makeSprite' vbo ebo thing wad progId spriteName' = do
  let spriteName = if (length (BS.unpack spriteName') == 4) then
                    findSpriteName wad spriteName'
                   else
                    spriteName'
      
  vaoId <- withNewPtr (glGenVertexArrays 1)
  glBindVertexArray vaoId

  vboId <- withNewPtr (glGenBuffers 1)
  glBindBuffer GL_ARRAY_BUFFER vboId
  withArrayLen vbo $ \len vertices ->
      glBufferData GL_ARRAY_BUFFER
                    (fromIntegral $ len * sizeOf (0 :: GLfloat))
                    (vertices :: Ptr GLfloat)
                    GL_STATIC_DRAW

  eboId <- withNewPtr (glGenBuffers 1)
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER eboId
  withArrayLen ebo $ \len vertices ->
      glBufferData GL_ELEMENT_ARRAY_BUFFER
                    (fromIntegral $ len * sizeOf (0 :: GLuint))
                    (vertices :: Ptr GLuint)
                    GL_STATIC_DRAW

  -- load sprite image
  let sprite = fromMaybe (error ("invalid sprite " ++ (BSC.unpack spriteName)))
          (M.lookup (mk spriteName) (WAD.wadSprites wad))
  let loadedPalette = loadPalettes wad
  p <- loadSprite sprite
  let spritePixels = unpackTuples (textureDataToColor loadedPalette p)
  let sW = fromIntegral $ WAD.pictureWidth $ WAD.spritePicture sprite
  let sH = fromIntegral $ WAD.pictureHeight $ WAD.spritePicture sprite
  texId <- withNewPtr (glGenTextures 1)
  glBindTexture GL_TEXTURE_2D texId

  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
  withArray spritePixels $
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) sW sH 0 GL_RGBA GL_FLOAT

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

  let v3 = if isNothing thing then
              (V3 (vbo !! 0) (vbo !! 1) (vbo !! 2))
           else let jt = fromJust thing in
              (V3 (fromIntegral $ WAD.thingX jt) 0.0 (fromIntegral $ WAD.thingY jt))

  Sprite <$> pure "Lev"
         <*> newIORef False
         <*> newIORef 0
         <*> pure renderData
         <*> pure v3
