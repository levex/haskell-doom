{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Shader.Language where
import Graphics.Shader.Types
import Data.List (intercalate)
import GHC.TypeLits
import Data.Proxy

data Exp
    = Var String
    | Scalar Double
    | forall t1 t2. (Expression t1, Expression t2) => Combine t1 t2
    | forall t. (Expression t) => Wrap t
    | FuncApp String [Exp]
    | Mul Exp Exp
    | Add Exp Exp
    | Negate Exp
    | Abs Exp
    | Signum Exp
    | forall t. (Expression t) => Assign Exp t

instance Num Exp where
    (*) = Mul
    (+) = Add
    abs = Abs
    signum = Signum
    fromInteger = Scalar . fromInteger
    negate (Negate n) = n
    negate n = Negate n

instance Show Exp where
    show (Var name)
        = name
    show (Combine e1 e2)
        = e1s ++ ", " ++ e2s
        where e1s = case toExpression e1 of
                        Combine e1' e1''
                            -> show e1' ++ ", " ++ show e1''
                        _   -> show e1
              e2s = case toExpression e2 of
                        Combine e2' e2''
                            -> show e2' ++ ", " ++ show e2''
                        _   -> show e2
    show (FuncApp funcname es)
        = funcname ++ "(" ++ intercalate ", " (map show es) ++ ")"
    show (Wrap e)
        = show e
    show (Mul e1 e2)
        = show e1 ++ " * " ++ show e2
    show (Add e1 e2)
        = show e1 ++ " + " ++ show e2
    show (Assign e1 e2)
        = show e1 ++ " = " ++ show e2
    show (Scalar s)
        = show s
    show (Abs e)
        = "abs(" ++ show e ++ ")"
    show (Signum e)
        = "signum(" ++ show e ++ ")"
    show (Negate s)
        = "-" ++ show s

class Show a => Expression a where
    fromExpression :: Exp -> a
    toExpression :: a -> Exp

data Vec (dim :: Nat)
    = Vec Exp

instance KnownNat n => Show (Vec n) where
    show (Vec expr)
        = case expr of
            expr'@Combine{} -> w expr'
            _               -> show expr
        where dim = natVal (Proxy :: Proxy n)
              w e = case dim of
                      1 -> show e
                      k -> "vec" ++ show k ++ "(" ++ show e ++ ")"

data Mat (rows :: Nat) (cols :: Nat)
    = Mat Exp

instance (KnownNat r, KnownNat c) => Show (Mat r c) where
    show (Mat expr)
        = case expr of
            expr'@Combine{} -> w expr'
            _               -> show expr
        where rdim = show $ natVal (Proxy :: Proxy r)
              cdim = show $ natVal (Proxy :: Proxy r)
              w e  = "mat" ++ rdim ++ "x" ++ cdim ++ "(" ++ show e ++ ")"

data Sampler (d :: Nat)
    = Sampler Exp
    deriving Show

instance (Show (Vec d), KnownNat d) => Expression (Vec d) where
    fromExpression = Vec
    toExpression (Vec e) = e

instance (Show (Mat r c), KnownNat r, KnownNat c) => Expression (Mat r c) where
    fromExpression = Mat
    toExpression (Mat e) = e

instance (Show (Sampler n), KnownNat n) => Expression (Sampler n) where
    fromExpression = Sampler
    toExpression (Sampler e) = e

-- ARGUMENT TYPES
type family FromArg a where
  FromArg 'Float = Vec 1
  FromArg 'Vec2 = Vec 2
  FromArg 'Vec3 = Vec 3
  FromArg 'Vec4 = Vec 4
  FromArg 'Mat1 = Mat 1 1
  FromArg 'Mat2 = Mat 2 2
  FromArg 'Mat3 = Mat 3 3
  FromArg 'Mat4 = Mat 4 4
  FromArg 'Sampler2D = Sampler 2

