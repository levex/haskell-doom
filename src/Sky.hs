{-# LANGUAGE OverloadedStrings #-}
module Sky where
import qualified Game.Waddle          as WAD
import           Foreign
import           Graphics.GL.Core33
import           GLUtils
import           Game
import           Var
import           Window
import           Sprite
import           TextureLoader

fillSkyTextureData :: WAD.Wad -> IO RenderData
fillSkyTextureData wad = do
    -- TODO: figure out which SKY texture
    skyVert <- loadShader GL_VERTEX_SHADER "src/shaders/sprite.vert"
    skyFrag <- loadShader GL_FRAGMENT_SHADER "src/shaders/sprite.frag"
    skyProgId <- glCreateProgram
    glAttachShader skyProgId skyVert
    glAttachShader skyProgId skyFrag
    glLinkProgram skyProgId
    glUseProgram skyProgId

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

    (tW, tH, txt) <- loadTexture wad "SKY1"
    texId <- withNewPtr (glGenTextures 1)
    glBindTexture GL_TEXTURE_2D texId

    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (fromIntegral GL_REPEAT)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)

    withArray txt $
      glTexImage2D GL_TEXTURE_2D 0 (fromIntegral GL_RGBA) tW tH 0 GL_RGBA GL_FLOAT
  
    posAttrib <- get $ AttribLocation skyProgId "position"
    glEnableVertexAttribArray posAttrib
    glVertexAttribPointer posAttrib
                          3
                          GL_FLOAT
                          (fromBool False)
                          (fromIntegral $ 5 * sizeOf (0 :: GLfloat))
                          nullPtr

    colAttrib <- get $ AttribLocation skyProgId "texcoord"
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
                          rdProg = skyProgId}
    where
      vbo = [-1.0,  1.0, 0.0,  0.0, 0.0,
              1.0,  1.0, 0.0,  1.0, 0.0,
             -1.0, -1.0, 0.0,  0.0, 1.0,
              1.0, -1.0, 0.0,  1.0, 1.0]

      ebo = [0, 1, 2,
             2, 1, 3]
