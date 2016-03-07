{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Graphics.Shader
    ( I.Shader(..)
    , I.showShader
    , T.GLSLType
    , T.GLTypeable(..)
    , T.TypeInfo(..)
    , I.Union
    , textureFrag
    , wallVert
    , spriteVert
    , floorFrag
    , floorVert
    , staticVert
    , glslTypeSize
    , Pos3
    , model
    , view
    , proj
    , Tex2) where
import Graphics.Shader.Internal as I
import Graphics.Shader.Types as T

-- TODO: swizzling

type Pos3 = 'Arg "position" 'Vec3
pos3 :: SVar Pos3
pos3 = SVar

type Tex2 = 'Arg "texcoord" 'Vec2
tex2 :: SVar Tex2
tex2 = SVar

type Texcoord = 'Arg "Texcoord" 'Vec2
texcoord :: SVar Texcoord
texcoord = SVar

type GlPos = 'Arg "gl_Position" 'Vec4
glPos :: SVar GlPos
glPos = SVar

type Model = 'Arg "model" 'Mat4
model :: SVar Model
model = SVar

type View = 'Arg "view" 'Mat4
view :: SVar View
view = SVar

type Proj = 'Arg "proj" 'Mat4
proj :: SVar Proj
proj = SVar

type Outcolor = 'Arg "outColor" 'Vec4
outcolor :: SVar Outcolor
outcolor = SVar

type TexSampler = 'Arg "tex" 'Sampler2D
texSampler :: SVar TexSampler
texSampler = SVar

wallVert :: Shader 150 '[Pos3, Tex2] '[Texcoord] '[Model, View, Proj] ()
wallVert = do
    out texcoord =: inp tex2
    var glPos =: (uni proj *: uni view *: uni model *: (inp pos3 &: float 1))

textureFrag :: Shader 150 '[Texcoord] '[Outcolor] '[TexSampler] ()
textureFrag
    = out outcolor =: texture (uni texSampler) (inp texcoord)

spriteVert :: Shader 150 '[Pos3, Tex2] '[Texcoord] '[Model, View, Proj] ()
spriteVert = do
    let tmpMv = SVar :: SVar ('Arg "tmp" 'Mat4)

    def tmpMv =: uni view *: uni model

    var tmpMv `at` _0 `at` _0 =: float 1
    var tmpMv `at` _0 `at` _1 =: float 0
    var tmpMv `at` _0 `at` _2 =: float 0

    var tmpMv `at` _2 `at` _0 =: float 0
    var tmpMv `at` _2 `at` _1 =: float 0
    var tmpMv `at` _2 `at` _2 =: float 1

    out texcoord =: inp tex2
    var glPos =: (uni proj *: var tmpMv *: (inp pos3 &: float 1))

staticVert :: Shader 150 '[Pos3, Tex2] '[Texcoord] '[] ()
staticVert = do
    out texcoord =: inp tex2
    var glPos    =: inp pos3 &: float 1.0

floorVert :: Shader 150 '[Pos3] '[] '[Model, View, Proj] ()
floorVert
    = var glPos =: uni proj *: uni view *: uni model *: (inp pos3 &: float 1)

floorFrag :: Shader 150 '[] '[Outcolor] '[] ()
floorFrag
    = out outcolor =: float 0.2 &: float 0.2 &: float 0.2 &: float 1.0
