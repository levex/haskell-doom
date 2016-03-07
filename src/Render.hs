module Render where
import Control.Monad.IO.Class
import Graphics.GL
import Linear
import qualified Game.Waddle as WAD

type Vertex2D = V2 GLfloat

vertexToVect :: WAD.Vertex -> V2 GLfloat
vertexToVect (WAD.Vertex x y)
    = V2 (-fromIntegral x / scale) (fromIntegral y / scale)

scale :: GLfloat
scale = 16

data RenderData = RenderData {
      rdVbo   :: GLuint
    , rdEbo   :: GLuint
    , rdVao   :: GLuint
    , rdTex   :: GLuint
    , rdProg  :: GLuint
    , rdExtra :: GLuint
}

bindRenderData :: MonadIO m => RenderData -> m ()
bindRenderData rd = do
  glUseProgram (rdProg rd)
  glBindVertexArray (rdVao rd)
  glBindBuffer GL_ELEMENT_ARRAY_BUFFER (rdEbo rd)
  glBindTexture GL_TEXTURE_2D (rdTex rd)

