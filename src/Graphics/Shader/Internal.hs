{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UndecidableInstances #-}
module Graphics.Shader.Internal where
import Graphics.Shader.Language
import Graphics.Shader.Types
import GHC.TypeLits
import Data.Proxy
import Control.Monad.Free
import Control.Monad.Writer
import Control.Applicative

data ExpFunctor a
    = Assignment Exp a
    deriving Functor

newtype Shader (ver :: Nat) (i :: [Arg]) (o :: [Arg]) (u :: [Arg]) a
    = Shader (Free ExpFunctor a)
    deriving (Functor, Applicative, Monad)

class (Functor f) => Print f where
    execPrint :: f (Writer [String] a) -> Writer [String] a

instance Print ExpFunctor where
    execPrint (Assignment expr cont)
        = tell [show expr ++ ";"] >> cont

-- Fold the free monad
foldShader :: (a -> b) -> (ExpFunctor b -> b) -> Shader v i o u a -> b
foldShader p i (Shader s)
    = foldFree' p i s
    where foldFree' pure' _ (Pure p')
              = pure' p'
          foldFree' pure' impure (Free i')
              = impure $ fmap (foldFree' pure' impure) i'

-- Show shader by extracting the type information and folding the free monad
showShader :: forall v i o u a.
              (KnownNat v, TypeInfo i, TypeInfo o, TypeInfo u) =>
              Shader v i o u a -> String
showShader shader
    = unlines (version : ins ++ unis ++ outs)
              ++ "\n"
              ++ "void main() {\n"
              ++ unlines (map ("\t" ++) body)
              ++ "}"
    where version = "#version " ++ show (natVal (Proxy :: Proxy v))
          ins  = extr "in"      (Proxy :: Proxy i)
          outs = extr "out"     (Proxy :: Proxy o)
          unis = extr "uniform" (Proxy :: Proxy u)
          body = snd . runWriter . foldShader pure execPrint $ shader

          extr what from
            = map (uncurry $ showT what) (extract from)
          showT w name typ
            = w ++ " " ++ show typ ++ " " ++ name ++ ";"

type LiftVar n i o u t =
         forall ver proxy.
         (KnownSymbol n, Expression (FromArg t)) =>
         proxy ('Arg n t) -> Shader ver i o u (FromArg t)

inp :: forall n i o u t. Elem ('Arg n t) i => LiftVar n i o u t
inp _ = pure $ fromExpression (Var name)
    where name = symbolVal (Proxy :: Proxy n)

out :: forall n i o u t. Elem ('Arg n t) o => LiftVar n i o u t
out _ = pure $ fromExpression (Var name)
    where name = symbolVal (Proxy :: Proxy n)

uni :: forall n i o u t. Elem ('Arg n t) u => LiftVar n i o u t
uni _ = pure $ fromExpression (Var name)
    where name = symbolVal (Proxy :: Proxy n)

var :: forall n i o u t. LiftVar n i o u t
var _ = pure $ fromExpression (Var name)
    where name = symbolVal (Proxy :: Proxy n)

float :: Applicative f => Double -> f Exp
float = pure . Scalar

-- Bilinear maps
class (Expression a, Expression b, Expression c) => Bilinear a b c | a b -> c where
    infixl 8 *::
    (*::) :: a -> b -> c
    a *:: b = fromExpression (Wrap a * Wrap b)
    infixl 8 *:
    (*:) :: Applicative f => f a -> f b -> f c
    (*:) = liftA2 (*::)

instance KnownNat n => Bilinear (Vec n) (Vec n) (Vec n)

instance (KnownNat a, KnownNat b, KnownNat c) =>
         Bilinear (Mat a b) (Mat b c) (Mat a c)

instance (KnownNat a, KnownNat b) => Bilinear (Mat a b) (Vec b) (Vec b)

-- Semigroups
class Expression a => Semigroup a where
    infixl 8 +::
    (+::) :: a -> a -> a
    a +:: b = fromExpression (Wrap a + Wrap b)
    infixl 8 +:
    (+:) :: Applicative f => f a -> f a -> f a
    (+:) = liftA2 (+::)

instance KnownNat n => Semigroup (Vec n)

instance (KnownNat r, KnownNat c) => Semigroup (Mat r c)

-- Things that can be turned into vectors
class (KnownNat (Size a)) => Vectorable a where
    fromVectorable :: (Size a ~ n) => a -> Vec n

instance Vectorable Exp where
    fromVectorable = Vec

instance KnownNat n => Vectorable (Vec n) where
    fromVectorable = id

type family Size v where
    Size (Vec n) = n
    Size Exp     = 1

class Combinable a b where
    type Combined a b
    (&::) :: a -> b -> Combined a b
    (&:) :: Applicative f => f a -> f b -> f (Combined a b)
    (&:) = liftA2 (&::)

instance (Vectorable a, Vectorable b) => Combinable a b where
    type Combined a b = Vec (Size a + Size b)
    v1 &:: v2 = Vec (Combine (fromVectorable v1) (fromVectorable v2))

class Assignable a where
    infixl 2 =::
    (=::) :: a -> a -> Shader v i o u ()
    infixl 2 =:
    (=:) :: Shader v i o u a -> Shader v i o u a -> Shader v i o u ()
    (=:) a b = join $ liftA2 (=::) a b

instance KnownNat n => Assignable (Vec n) where
    (Vec v) =:: val = imp $ Assignment (Assign v val) ()

instance (KnownNat r, KnownNat c) => Assignable (Mat r c) where
    (Mat v) =:: val = imp $ Assignment (Assign v val) ()

texture :: (KnownNat n, Applicative f) => f (Sampler m) -> f (Vec n) -> f (Vec 4)
texture = liftA2 func
    where func (Sampler s) v = Vec (FuncApp "texture" [s, Wrap v])

imp :: ExpFunctor a -> Shader v i o u a
imp c = Shader (Free (fmap Pure c))

-- Constraint is only satisifed when x is an element of the type-level list xs
class Elem x xs

instance Elem x xs => Elem x (y ': xs)
instance {-# OVERLAPPING #-} Elem x (x ': xs)

-- Append two lists together, maybe should nub?
type family (Union (l1 :: [k]) (l2 :: [k])) where
    Union l '[] = l
    Union l1 (a ': l2) = Union (a ': l1) l2
