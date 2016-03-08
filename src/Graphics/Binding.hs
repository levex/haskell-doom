{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
--{-# LANGUAGE PolyKinds #-}

module Graphics.Binding where
import Control.Monad
import Control.Monad.IO.Class
import Data.Proxy
import Data.Var
import Foreign
import Foreign.C.String
import Graphics.GL
import Graphics.Program
import Graphics.Shader
import Graphics.Shader.Types
import Linear
import Graphics.TupleList

data Bindable (k :: [Arg]) a = Bindable [a]

type AsData x = FromList (ArgMap x)

type family ArgMap (xs :: [Arg]) :: [*] where
    ArgMap '[] = '[]
    ArgMap (x ': xs) = (ArgToLinear x ': ArgMap xs)

bindVertexData :: (x ~ AsData i, ToList x GLfloat, TypeInfo i, MonadIO m) => Program i u -> [x] -> m ()
bindVertexData p xs = bindVertexData' p b
    where b = Bindable $ concatMap toList' xs :: Bindable i GLfloat

bindVertexData' :: forall a i u m.
    (Storable a, GLTypeable a, TypeInfo i, MonadIO m) =>
    Program i u -> Bindable i a -> m ()
bindVertexData' (Program progId) (Bindable bdata) = liftIO $ do
    withArrayLen bdata $ \len vertices ->
        glBufferData GL_ARRAY_BUFFER
                     (fromIntegral $ len * dataSize)
                     (vertices :: Ptr a)
                     GL_STATIC_DRAW
    foldM_ (\offset (name, size) -> do
            attrib <- fromIntegral <$>
                        withCString name (glGetAttribLocation progId)
            glEnableVertexAttribArray attrib
            glVertexAttribPointer attrib
                                  size
                                  (glType proxy)
                                  (fromBool False)
                                  (fromIntegral $ totalSize * dataSize)
                                  offset
            return (offset `plusPtr` fromIntegral (size * fromIntegral dataSize))
        ) nullPtr (map (fmap (fromIntegral . glslTypeSize)) extracted)
    where extracted = extract (Proxy :: Proxy i)
          totalSize = fromIntegral $ sum . map (glslTypeSize . snd) $ extracted
          dataSize  = sizeOf proxy
          proxy   = undefined :: a

-- Uniform binding
data Uniform u (v :: Arg) a = forall i proxy. Uniform (Program u i) (proxy v)

instance (MonadIO m, HasBinder a, TypeInfo '[v], Compatible v a) => HasSetter m (Uniform u v a) a where
    (Uniform (Program progId) _) $= uniData = liftIO $ do
        let (name, _) = head $ extract (Proxy :: Proxy '[v])
        uniId <- withCString name $ glGetUniformLocation progId
        bindUniform uniData (fromIntegral uniId) 1

class Storable v => HasBinder v where
    bindFunc :: v -> GLint -> GLsizei -> Ptr a -> IO ()
    bindUniform :: MonadIO m => v -> GLint -> GLsizei -> m ()
    bindUniform val loc count = liftIO $
            with val $ \trans -> bindFunc val loc count trans

instance HasBinder (M44 GLfloat) where
    bindFunc _ = matrixBinder glUniformMatrix4fv

instance HasBinder (M33 GLfloat) where
    bindFunc _ = matrixBinder glUniformMatrix3fv

instance HasBinder (M23 GLfloat) where
    bindFunc _ = matrixBinder glUniformMatrix2x3fv
-- TODO: add other matrices

instance HasBinder (V4 GLfloat) where
    bindFunc _ = vectorBinder glUniform4fv

instance HasBinder (V3 GLfloat) where
    bindFunc _ = vectorBinder glUniform3fv

instance HasBinder (V2 GLfloat) where
    bindFunc _ = vectorBinder glUniform2fv

instance HasBinder (V1 GLfloat) where
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

