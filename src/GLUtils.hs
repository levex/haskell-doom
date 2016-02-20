{-# LANGUAGE FlexibleInstances,
             ScopedTypeVariables,
             FlexibleContexts,
             DataKinds,
             TypeFamilies,
             MultiParamTypeClasses,
             ExistentialQuantification #-}
module GLUtils where
import Var
import Graphics.GL.Core33
import Control.Monad.IO.Class
import Foreign.C.String
import Foreign
import Linear
import Control.Monad

type ProgId = GLuint

-- Attribute location
data AttribLocation a = AttribLocation ProgId String

instance (MonadIO m, Num a) => HasGetter m (AttribLocation a) a where
    get (AttribLocation progId name) = liftIO $
        fromIntegral <$> withCString name (glGetAttribLocation progId)

-- Uniform binding
data Uniform a = Uniform ProgId String

instance (MonadIO m, UniformBinding a, Storable a) => HasSetter m (Uniform a) a where
    (Uniform progId name) $= uniData = liftIO $ do
        uniId <- withCString name $ glGetUniformLocation progId
        bindUniform uniData (fromIntegral uniId) 1

class Storable v => UniformBinding v where
    bindFunc :: v -> GLint -> GLsizei -> Ptr a -> IO ()
    bindUniform :: MonadIO m => v -> GLint -> GLsizei -> m ()
    bindUniform val loc count = liftIO $
            with val $ \trans -> bindFunc val loc count trans

instance UniformBinding (M44 GLfloat) where
    bindFunc _ = matrixBinder glUniformMatrix4fv

instance UniformBinding (M33 GLfloat) where
    bindFunc _ = matrixBinder glUniformMatrix3fv

instance UniformBinding (M23 GLfloat) where
    bindFunc _ = matrixBinder glUniformMatrix2x3fv
-- TODO: add other matrices

instance UniformBinding (V4 GLfloat) where
    bindFunc _ = vectorBinder glUniform4fv

instance UniformBinding (V3 GLfloat) where
    bindFunc _ = vectorBinder glUniform3fv

instance UniformBinding (V2 GLfloat) where
    bindFunc _ = vectorBinder glUniform2fv

instance UniformBinding (V1 GLfloat) where
    bindFunc _ = vectorBinder glUniform1fv
-- TODO: add other vectors (GLint)

matrixBinder :: MonadIO m =>
                (GLint -> GLsizei -> GLboolean -> Ptr a -> IO ()) ->
                GLint -> GLsizei -> Ptr b -> m ()
matrixBinder f loc count val
    = liftIO $ f loc count (fromBool True) (castPtr val)

vectorBinder :: MonadIO m =>
                (GLint -> GLsizei -> Ptr a -> IO ()) ->
                GLint -> GLsizei -> Ptr b -> m ()
vectorBinder f loc count val
    = liftIO $ f loc count (castPtr val)

-- Misc aux. functions
offsetPtr :: Storable a => Int -> a -> Ptr GLvoid
offsetPtr x s = plusPtr nullPtr (fromIntegral $ x * sizeOf s)

withNewPtr :: Storable a => (Ptr a -> IO b) -> IO a
withNewPtr f = alloca (\p -> f p >> get p)

loadShader :: GLenum -> FilePath -> IO GLuint
loadShader shaderTypeFlag filePath = do
  code <- readFile filePath
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

-- Fragment shader

data FragmentShaderField
    = FragDiffuseColor  -- 0
    | FragMaterialID    -- 1
    | FragSpecularColor -- 2
    | FragPosition      -- 3
    | FragNormal        -- 4
    deriving Enum

data FragShaderLocation = FragShaderLocation ProgId String

instance MonadIO m => HasSetter m FragShaderLocation FragmentShaderField where
    (FragShaderLocation progId name) $= loc
        = liftIO . withCString name $
            glBindFragDataLocation progId (fromIntegral $ fromEnum loc)

-- GL typeable stuff
class GLTypeable a where
    glType :: a -> Word32

instance GLTypeable GLfloat where
    glType _ = GL_FLOAT

instance GLTypeable GLint where
    glType _ = GL_INT

-- TODO: etc
