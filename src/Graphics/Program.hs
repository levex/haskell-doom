{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Graphics.Program where

import Graphics.Shader
import Graphics.Shader.Types
import GHC.TypeLits
import Graphics.GL
import Graphics.GLUtils

type ProgId = GLuint

data Program (inputs   :: [Arg])
             (uniforms :: [Arg])
             = Program ProgId

mkProgram :: ( KnownNat ver
             , TypeInfo i
             , TypeInfo o
             , TypeInfo u
             , TypeInfo o'
             , TypeInfo u' ) =>
             Shader ver i o u a -> Shader ver o o' u' a' -> IO (Program i (Union u u'))
mkProgram vert frag = do
    progId <- glCreateProgram
    vertS <- shaderFromString GL_VERTEX_SHADER (showShader vert)
    fragS <- shaderFromString GL_FRAGMENT_SHADER (showShader frag)
    glAttachShader progId vertS
    glAttachShader progId fragS

    glLinkProgram progId
    glUseProgram progId
    glDeleteShader vertS
    glDeleteShader fragS
    return $ Program progId
