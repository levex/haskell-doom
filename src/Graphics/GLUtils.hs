module Graphics.GLUtils where
import Data.Var
import Graphics.GL
import Foreign.C.String
import Foreign
import Control.Monad

-- Misc aux. functions
offsetPtr :: Storable a => Int -> a -> Ptr GLvoid
offsetPtr x s = plusPtr nullPtr (fromIntegral $ x * sizeOf s)

withNewPtr :: Storable a => (Ptr a -> IO b) -> IO a
withNewPtr f = alloca (\p -> f p >> get p)

shaderFromString :: GLenum -> String -> IO GLuint
shaderFromString shaderTypeFlag code = do
    shader <- glCreateShader shaderTypeFlag
    withCString code $ \codePtr ->
      with codePtr $ \codePtrPtr ->
        glShaderSource shader 1 codePtrPtr nullPtr
    glCompileShader shader
    status <- toBool <$> withNewPtr (glGetShaderiv shader GL_COMPILE_STATUS)
    unless status $
        alloca $ \err -> do
            glGetShaderInfoLog shader 512 nullPtr err
            err' <- peekCString err
            error err'
    return shader


loadShader :: GLenum -> FilePath -> IO GLuint
loadShader shaderTypeFlag filePath = do
  code <- readFile filePath
  shaderFromString shaderTypeFlag code

