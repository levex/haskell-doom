{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Graphics.Shader.Types where
import Data.Proxy
import GHC.TypeLits
import Graphics.GL
import Data.Int
import Data.Word
import Linear

data Arg = Arg Symbol GLSLType

-- Convert between Haskell types and GL scalar primitives
class GLTypeable a where
    glType :: a -> GLenum

instance GLTypeable Int where
    glType _ = GL_INT

instance GLTypeable Float where
    glType _ = GL_FLOAT

instance GLTypeable Double where
    glType _ = GL_DOUBLE

instance GLTypeable Word8 where
    glType _ = GL_UNSIGNED_BYTE

instance GLTypeable Word16 where
    glType _ = GL_UNSIGNED_SHORT

instance GLTypeable Word32 where
    glType _ = GL_UNSIGNED_INT

instance GLTypeable Int8 where
    glType _ = GL_BYTE

instance GLTypeable Int16 where
    glType _ = GL_SHORT

-- Poly-kinded proxy for the shader variables used in the DSL
data SVar (a :: k) = SVar

data GLSLType
    = Float
    | Vec2
    | Vec3
    | Vec4
    | Mat1
    | Mat2
    | Mat3
    | Mat4
    | Sampler2D

-- Compatibility with the respective types from the Linear library
class Compatible a b

instance (FromLinear a ~ typ) => Compatible a ('Arg name typ)
instance (FromLinear a ~ typ) => Compatible ('Arg name typ) a

type family FromLinear a where
    FromLinear (M22 a) = 'Mat2
    FromLinear (M33 a) = 'Mat3
    FromLinear (M44 a) = 'Mat4
    FromLinear (V2 a)  = 'Vec2
    FromLinear (V3 a)  = 'Vec3
    FromLinear (V4 a)  = 'Vec4

instance Show GLSLType where
    show Float     = "float"
    show Vec2      = "vec2"
    show Vec3      = "vec3"
    show Vec4      = "vec4"
    show Mat1      = "mat1"
    show Mat2      = "mat2"
    show Mat3      = "mat3"
    show Mat4      = "mat4"
    show Sampler2D = "sampler2D"

glslTypeSize :: GLSLType -> Int
glslTypeSize Float = 1
glslTypeSize Vec2 = 2
glslTypeSize Vec3 = 3
glslTypeSize Vec4 = 4
glslTypeSize Mat1 = 1
glslTypeSize Mat2 = 4
glslTypeSize Mat3 = 9
glslTypeSize Mat4 = 16
glslTypeSize _    = undefined

-- Extract type information from a list of type-level tuples.
-- The shader variables are stored in this format at the type-level
class TypeInfo (f :: [Arg]) where
    extract :: proxy f -> [(String, GLSLType)]

instance TypeInfo '[] where
    extract _ = []

instance (KnownSymbol sym, KnownGLSLType arg, TypeInfo xs) =>
    TypeInfo (('Arg sym arg) ': xs) where
    extract _ = ( symbolVal (Proxy :: Proxy sym)
                , argVal (Proxy :: Proxy arg)
                ) : extract (Proxy :: Proxy xs)

-- Get back the term-level constructor from the promoted ones
class KnownGLSLType v where
    argVal :: proxy v -> GLSLType
instance KnownGLSLType 'Float where
    argVal _ = Float
instance KnownGLSLType 'Vec2 where
    argVal _ = Vec2
instance KnownGLSLType 'Vec3 where
    argVal _ = Vec3
instance KnownGLSLType 'Vec4 where
    argVal _ = Vec4
instance KnownGLSLType 'Mat1 where
    argVal _ = Mat1
instance KnownGLSLType 'Mat2 where
    argVal _ = Mat2
instance KnownGLSLType 'Mat3 where
    argVal _ = Mat3
instance KnownGLSLType 'Mat4 where
    argVal _ = Mat4
instance KnownGLSLType 'Sampler2D where
    argVal _ = Sampler2D
