{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
module Render where

import Graphics.GL
import Linear
import Graphics.Program
import qualified Game.Waddle as WAD
import Control.Monad.IO.Class

type Vertex2D = V2 GLfloat

vertexToVect :: WAD.Vertex -> V2 GLfloat
vertexToVect (WAD.Vertex x y)
    = V2 (-fromIntegral x / scale) (fromIntegral y / scale)

scale :: GLfloat
scale = 16

data RenderData = forall i u. RenderData {
      rdVbo   :: GLuint
    , rdEbo   :: GLuint
    , rdVao   :: GLuint
    , rdTex   :: GLuint
    , rdProg  :: Program i u
    , rdExtra :: GLuint
}

bindRenderData :: MonadIO m => RenderData -> m ()
bindRenderData RenderData{..} = do
  let (Program progId) = rdProg
  glUseProgram progId
  glBindVertexArray rdVao
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER rdEbo
  glBindTexture GL_TEXTURE_2D rdTex

