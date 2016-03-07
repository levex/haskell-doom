{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

{- GENERATED -}
module Graphics.TupleList where
import Data.Foldable (toList)

type family FromList (a :: [*]) where
  FromList '[a1] = a1
  FromList '[a1, a2] = (a1, a2)
  FromList '[a1, a2, a3] = (a1, a2, a3)
  FromList '[a1, a2, a3, a4] = (a1, a2, a3, a4)
  FromList '[a1, a2, a3, a4, a5] = (a1, a2, a3, a4, a5)
  FromList '[a1, a2, a3, a4, a5, a6] = (a1, a2, a3, a4, a5, a6)
  FromList '[a1, a2, a3, a4, a5, a6, a7] = (a1, a2, a3, a4, a5, a6, a7)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8] = (a1, a2, a3, a4, a5, a6, a7, a8)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8, a9] = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
  FromList '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)

class ToList a b where
  toList' :: a -> [b]

instance (Foldable t1) => ToList (t1 a) a where
  toList' = toList
instance (Foldable t1, Foldable t2) => ToList (t1 a, t2 a) a where
  toList' (a1, a2) = toList a1 ++ toList a2
instance (Foldable t1, Foldable t2, Foldable t3) => ToList (t1 a, t2 a, t3 a) a where
  toList' (a1, a2, a3) = toList a1 ++ toList a2 ++ toList a3
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4) => ToList (t1 a, t2 a, t3 a, t4 a) a where
  toList' (a1, a2, a3, a4) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a) a where
  toList' (a1, a2, a3, a4, a5) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a) a where
  toList' (a1, a2, a3, a4, a5, a6) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8, Foldable t9) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a, t9 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8, a9) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8 ++ toList a9
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8, Foldable t9, Foldable t10) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a, t9 a, t10 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8 ++ toList a9 ++ toList a10
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8, Foldable t9, Foldable t10, Foldable t11) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a, t9 a, t10 a, t11 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8 ++ toList a9 ++ toList a10 ++ toList a11
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8, Foldable t9, Foldable t10, Foldable t11, Foldable t12) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a, t9 a, t10 a, t11 a, t12 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8 ++ toList a9 ++ toList a10 ++ toList a11 ++ toList a12
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8, Foldable t9, Foldable t10, Foldable t11, Foldable t12, Foldable t13) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a, t9 a, t10 a, t11 a, t12 a, t13 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8 ++ toList a9 ++ toList a10 ++ toList a11 ++ toList a12 ++ toList a13
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8, Foldable t9, Foldable t10, Foldable t11, Foldable t12, Foldable t13, Foldable t14) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a, t9 a, t10 a, t11 a, t12 a, t13 a, t14 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8 ++ toList a9 ++ toList a10 ++ toList a11 ++ toList a12 ++ toList a13 ++ toList a14
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8, Foldable t9, Foldable t10, Foldable t11, Foldable t12, Foldable t13, Foldable t14, Foldable t15) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a, t9 a, t10 a, t11 a, t12 a, t13 a, t14 a, t15 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8 ++ toList a9 ++ toList a10 ++ toList a11 ++ toList a12 ++ toList a13 ++ toList a14 ++ toList a15
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8, Foldable t9, Foldable t10, Foldable t11, Foldable t12, Foldable t13, Foldable t14, Foldable t15, Foldable t16) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a, t9 a, t10 a, t11 a, t12 a, t13 a, t14 a, t15 a, t16 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8 ++ toList a9 ++ toList a10 ++ toList a11 ++ toList a12 ++ toList a13 ++ toList a14 ++ toList a15 ++ toList a16
instance (Foldable t1, Foldable t2, Foldable t3, Foldable t4, Foldable t5, Foldable t6, Foldable t7, Foldable t8, Foldable t9, Foldable t10, Foldable t11, Foldable t12, Foldable t13, Foldable t14, Foldable t15, Foldable t16, Foldable t17) => ToList (t1 a, t2 a, t3 a, t4 a, t5 a, t6 a, t7 a, t8 a, t9 a, t10 a, t11 a, t12 a, t13 a, t14 a, t15 a, t16 a, t17 a) a where
  toList' (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) = toList a1 ++ toList a2 ++ toList a3 ++ toList a4 ++ toList a5 ++ toList a6 ++ toList a7 ++ toList a8 ++ toList a9 ++ toList a10 ++ toList a11 ++ toList a12 ++ toList a13 ++ toList a14 ++ toList a15 ++ toList a16 ++ toList a17
