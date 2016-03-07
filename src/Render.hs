{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
module Render where
import Control.Monad.IO.Class
import Graphics.GL
import Linear
import Graphics.Program
import qualified Game.Waddle as WAD

type Vertex2D = V2 GLfloat

vertexToVect :: WAD.Vertex -> V2 GLfloat
vertexToVect (WAD.Vertex x y)
    = V2 (-fromIntegral x / scale) (fromIntegral y / scale)

scale :: GLfloat
scale = 16

data RenderData = forall u i. RenderData {
      rdVbo   :: GLuint
    , rdEbo   :: GLuint
    , rdVao   :: GLuint
    , rdTex   :: GLuint
    , rdProg  :: Program u i
    , rdExtra :: GLuint
}

bindRenderData :: MonadIO m => RenderData -> m ()
bindRenderData RenderData{..} = do
  let (Program progId) = rdProg
  glUseProgram progId
  glBindVertexArray rdVao
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER rdEbo
  glBindTexture GL_TEXTURE_2D rdTex

