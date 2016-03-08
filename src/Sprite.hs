{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
module Sprite (
      createLevelThings
    , loadSpriteColor
    , Sprite(..)
    )
    where
import           Control.Monad
import           Data.CaseInsensitive hiding (map)
import           Data.IORef
import           Data.Array.IO         as AI
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Char
import           Foreign
import qualified Game.Waddle           as WAD
import           Graphics.GLUtils
import           Graphics.Program
import           Graphics.Shader
import           Graphics.GL.Core33
import           Linear
import           TextureLoader
import           SpriteMap
import           Graphics.Binding
import           Render
import           Types

type SpriteArgs = '[Pos3, VertexPos, Tex2]

data TempSprite = TempSprite {
          tempSpriteVbo   :: [AsData SpriteArgs]
        , tempSpriteEbo   :: [GLuint]
        , tempSpriteTexId :: GLuint
        , tempSpritePos   :: V3 GLfloat
    }

data Sprite = Sprite {
        spriteName       :: String,     -- sprite name in WAD
        spriteActive     :: IORef Bool, -- whether we can start moving
        spriteAnimFrame  :: IORef Int,  -- current animation frame
        spriteRenderData :: RenderData,
        spritePos        :: Pos
    }

loadSpriteColor :: WAD.Sprite -> ColorPalette -> IO [V4 GLfloat]
loadSpriteColor sprite cp
  = textureDataToColor cp <$> loadSprite sprite

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
        let ty = fromIntegral (WAD.postTop post) + i
        when (tx <= fW - 1 && ty <= fH - 1 && tx >= 0 && ty >= 0) $
          writeArray pxArr (tx + ty * fW) pt
  getElems pxArr

createLevelThings :: WAD.Wad -> Program SpriteArgs i -> [WAD.Thing] -> IO [Sprite]
createLevelThings wad program things = do
    let notReserved = nonReservedSprites things

    (things', _) <- foldM (\(spdata, textures) t -> do
            let ttype = WAD.thingType t

            (textures', texId) <- case M.lookup ttype textures of
                Just texId' -> return (textures, texId')
                Nothing     -> do
                    texId' <- loadSpriteImage wad ttype
                    return (M.insert ttype texId' textures, texId')
            return (createTempSprite t texId : spdata, textures')
          ) ([], M.empty) notReserved
    forM things' (makeSprite program)

createTempSprite :: WAD.Thing -> GLuint -> TempSprite
createTempSprite WAD.Thing{..} texId
    = TempSprite vbo ebo texId pos
        where pW = 2 -- FIXME, ugly
              pH = 3 -- FIXME, ugly
              tx = fromIntegral thingX / scale
              ty = fromIntegral thingY / scale
              vbo = [ (V3 (-tx) pH ty, V1 (-pW), V2 1 0)
                    , (V3 (-tx) pH ty, V1    pW, V2 0 0)
                    , (V3 (-tx) 0  ty, V1 (-pW), V2 1 1)
                    , (V3 (-tx) 0  ty, V1    pW, V2 0 1)
                    ]
              ebo = [
                0, 1, 2,
                2, 1, 3]
              pos = V3 tx 0 ty

findSpriteName :: WAD.Wad -> WAD.LumpName -> WAD.LumpName
findSpriteName wad name
  = findSpriteName' "A" "0"
  where
    findSpriteName' f@(a : as) g@(b : bs)
        = case M.lookup (mk t) (WAD.wadSprites wad) of
            Just _  -> t
            Nothing -> findSpriteName' (na : as) (nb : bs)
      where
        t = BS.append (BS.append name (BSC.pack f)) (BSC.pack g)
        na = chr $ ord a + 1
        nb = chr $ ord b + 1

makeSprite :: Program SpriteArgs i -> TempSprite -> IO Sprite
makeSprite program TempSprite{..} = do
  vaoId <- withNewPtr (glGenVertexArrays 1)
  glBindVertexArray vaoId

  vboId <- withNewPtr (glGenBuffers 1)
  glBindBuffer GL_ARRAY_BUFFER vboId

  bindVertexData program tempSpriteVbo

  eboId <- withNewPtr (glGenBuffers 1)
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER eboId
  withArrayLen tempSpriteEbo $ \len vertices ->
      glBufferData GL_ELEMENT_ARRAY_BUFFER
                    (fromIntegral $ len * sizeOf (0 :: GLuint))
                    (vertices :: Ptr GLuint)
                    GL_STATIC_DRAW

  let renderData = RenderData {
                       rdVao = vaoId
                     , rdVbo = vboId
                     , rdTex = tempSpriteTexId
                     , rdProg = program
                     , rdEbo = eboId
                     , rdExtra = 0
                     }

  Sprite <$> pure "Lev"
         <*> newIORef False
         <*> newIORef 0
         <*> pure renderData
         <*> pure tempSpritePos


loadSpriteImage :: WAD.Wad -> WAD.ThingType -> IO GLuint
loadSpriteImage wad tt = do
  let sn' = thingToSprite tt
      sn = if length (BS.unpack sn') == 4 then
                    findSpriteName wad sn'
                   else
                    sn'

  -- load sprite image
  let sprite = fromMaybe (error ("invalid sprite " ++ BSC.unpack sn))
          (M.lookup (mk sn) (WAD.wadSprites wad))
  let loadedPalette = loadPalettes wad
  p <- loadSprite sprite
  let spritePixels = textureDataToColor loadedPalette p
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
  return texId
