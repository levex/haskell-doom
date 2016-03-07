{-# LANGUAGE OverloadedStrings #-}
module Sky where
import qualified Game.Waddle as WAD
import Foreign
import Graphics.GL.Core33
import Graphics.GLUtils
import Graphics.Shader
import Game
import TextureLoader
import Graphics.Binding
import Graphics.Program
import Render

fillSkyTextureData :: WAD.Wad -> IO RenderData
fillSkyTextureData wad = do
    -- TODO: figure out which SKY texture
    skyProgram@(Program skyProgId) <- mkProgram spriteVert spriteFrag
    glUseProgram skyProgId

    vaoId <- withNewPtr (glGenVertexArrays 1)
    glBindVertexArray vaoId

    vboId <- withNewPtr (glGenBuffers 1)
    glBindBuffer GL_ARRAY_BUFFER vboId

    bindVertexData skyProgram (Bindable vbo)

    eboId <- withNewPtr (glGenBuffers 1)
    glBindBuffer GL_ELEMENT_ARRAY_BUFFER eboId
    withArrayLen ebo $ \len vertices ->
      glBufferData GL_ELEMENT_ARRAY_BUFFER
                    (fromIntegral $ len * sizeOf (0 :: GLuint))
                    (vertices :: Ptr GLuint)
                    GL_STATIC_DRAW

    (tW, tH, txt) <- loadTexture wad "SKY1"
    texId <- withNewPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D texId

    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)

    withArray txt $
      glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) tW tH 0 GL_RGBA GL_FLOAT

    return $ RenderData { rdVbo = vboId,
                          rdEbo = eboId,
                          rdTex = texId,
                          rdVao = vaoId,
                          rdProg = skyProgId}
    where
      vbo = [-1.0,  1.0, 0.0,  0.0, 0.0,
              1.0,  1.0, 0.0,  1.0, 0.0,
             -1.0, -1.0, 0.0,  0.0, 1.0,
              1.0, -1.0, 0.0,  1.0, 1.0] :: [Float]

      ebo = [0, 1, 2,
             2, 1, 3]
