{-# LANGUAGE OverloadedStrings #-}
module UI where

import qualified Data.Map             as M
import qualified Data.ByteString      as BS
import qualified Game.Waddle          as WAD
import           Foreign
import           Graphics.GL.Core33
import           Data.Maybe
import           Data.CaseInsensitive hiding (map)
import           Control.Monad.Reader

import GLUtils
import Window
import Var
import Game

uiTest :: Game RenderData
uiTest = do
  wad'     <- asks wad
  palette' <- asks palette
  liftIO $ uiTestIO wad' palette'

-- ugly af
uiTestIO :: WAD.Wad -> ColorPalette -> IO RenderData
uiTestIO wad' palette = do
  let stbarLump = fromMaybe (error "UI not found")
                (M.lookup (mk "STBAR") (WAD.wadLumpLookup wad'))

  stbarVert <- liftIO $ loadShader GL_VERTEX_SHADER "src/shaders/sprite.vert"
  stbarFrag <- liftIO $ loadShader GL_FRAGMENT_SHADER "src/shaders/sprite.frag"
  stbarProgId <- glCreateProgram
  glAttachShader stbarProgId stbarVert
  glAttachShader stbarProgId stbarFrag
  glLinkProgram stbarProgId
  glUseProgram stbarProgId

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

  let (tW, tH, txt) = (320, 200, BS.unpack stbarLump)
  texId <- withNewPtr (glGenTextures 1)
  glBindTexture GL_TEXTURE_2D texId

  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)

  withArray txt $
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) tW tH 0 GL_RGBA GL_FLOAT

  posAttrib <- get $ AttribLocation stbarProgId "position"
  glEnableVertexAttribArray posAttrib
  glVertexAttribPointer posAttrib
                        3
                        GL_FLOAT
                        (fromBool False)
                        (fromIntegral $ 5 * sizeOf (0 :: GLfloat))
                        nullPtr

  colAttrib <- get $ AttribLocation stbarProgId "texcoord"
  glEnableVertexAttribArray colAttrib
  glVertexAttribPointer colAttrib
                        2
                        GL_FLOAT
                        (fromBool False)
                        (fromIntegral $ 5 * sizeOf (0 :: GLfloat))
                        (offsetPtr 3 (0 :: GLfloat))

  return $ RenderData { rdVbo = vboId,
                        rdEbo = eboId,
                        rdTex = texId,
                        rdVao = vaoId,
                        rdProg = stbarProgId}
  where
    vbo = [-1.0,  1.0, 0.0,  0.0, 0.0,
            1.0,  1.0, 0.0,  1.0, 0.0,
           -1.0, -1.0, 0.0,  0.0, 1.0,
            1.0, -1.0, 0.0,  1.0, 1.0]

    ebo = [0, 1, 2,
           2, 1, 3]
    
