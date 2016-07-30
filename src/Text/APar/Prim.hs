{-|

Module      : Text.APar.Prim
CopyRight   : (c) Junyoung Clare Jang
License     : BSD-style (see the LICENSE FILE)

Maintainer  : Junyoung Clare Jang <jjc9310@gmail.com>
Stability   : testing
Portability : portable

Primitives and Simple Combinators of APar parser library

-}


module Text.APar.Prim
  (

  )
where

import Prelude hiding (id, (.))

import Data.List

import Control.Category as C
import Control.Arrow as A

{-|
Static parser for checking first character of input stream
-}
data StaticPar s = SP Bool [s]

emptySP :: StaticPar s
emptySP = SP True []
{-# INLINE emptySP #-}

zeroSP :: StaticPar s
zeroSP = SP False []
{-# INLINE zeroSP #-}

(<++>) :: Eq s => StaticPar s -> StaticPar s -> StaticPar s
(<++>) = \(SP e1 ss1) (SP e2 ss2) -> SP
                                     (e1 || e2)
                                     (ss1 ++ ss2)
{-# INLINE (<++>) #-}

(<>>>) :: Eq s => StaticPar s -> StaticPar s -> StaticPar s
(<>>>) = \(SP e1 ss1) (SP e2 ss2) -> SP
                                     (e1 && e2)
                                     (ss1 `union` if e1
                                                  then ss2
                                                  else [])
{-# INLINE (<>>>) #-}


data DynamicPar s b c = DP ((b, [s]) -> (c, [s]))

instance Category (DynamicPar s) where

  id                = DP id
  {-# INLINE id #-}
  
  DP dpf2 . DP dpf1 = DP $ dpf2 . dpf1
  {-# INLINE (.) #-}

instance Arrow (DynamicPar s) where

  arr f =
    DP $ \(b, xs) -> (f b, xs)
  {-# INLINE arr #-}

  first (DP dpf) =
    DP $ \((b, d), xs) ->
           let (c, xs') = dpf (b, xs)
           in ((c, d), xs')
  {-# INLINE first #-}
           
  second (DP dpf) =
    DP $ \((d, b), xs) ->
           let (c, xs') = dpf (b, xs)
           in ((d, c), xs')
  {-# INLINE second #-}

  DP dpf1 *** DP dpf2 =
    DP $ \((b, b'), xs) ->
           let (c, xs') = dpf1 (b, xs)
               (c', xs'') = dpf2 (b', xs')
           in ((c, c'), xs'')
  {-# INLINE (***) #-}

  DP dpf1 &&& DP dpf2 =
    DP $ \(b, xs) ->
           let (c, xs') = dpf1 (b, xs)
               (c', xs'') = dpf2 (b, xs')
           in ((c, c'), xs'')
  {-# INLINE (&&&) #-}


instance ArrowZero (DynamicPar s) where
  zeroArrow = DP $ \_ -> undefined
  {-# INLINE zeroArrow #-}


data FullPar s b c = FP {sP::(StaticPar s), dP::(DynamicPar s b c)}

instance (Eq s) => Category (FullPar s) where
  
  -- id :: FullPar s a a
  id =
    FP emptySP id
                            
  -- (.) :: FullPar s b c -> FullPar s a b -> FullPar s a c
  FP sp2 dp2 . FP sp1 dp1 =
    FP (sp1 <>>> sp2) (dp2 . dp1)


instance (Eq s) => Arrow (FullPar s) where
  
  arr f =
    FP emptySP (arr f)
    
  first (FP sp dp) =
    FP sp (first dp)
    
  second (FP sp dp) =
    FP sp (second dp)
    
  FP sp1 dp1 *** FP sp2 dp2 =
    FP (sp1 <>>> sp2) (dp1 *** dp2)
    
  FP sp1 dp1 &&& FP sp2 dp2 =
    FP (sp1 <>>> sp2) (dp1 &&& dp2)


instance (Eq s) => ArrowZero (FullPar s) where

  zeroArrow =
    FP zeroSP zeroArrow


instance (Eq s) => ArrowPlus (FullPar s) where

  FP sp1@(SP e1 ss1) (DP dpf1) <+> FP sp2@(SP _ ss2) (DP dpf2) =
    FP (sp1 <++> sp2) (DP $ \(b, xs) ->
                              case xs of
                                []  -> if e1 then dpf1 (b, []) else dpf2 (b, [])
                                x:_ -> if x `elem` ss1 then dpf1 (b, xs) else
                                         if x `elem` ss2 then dpf2 (b, xs) else
                                           if e1 then dpf1 (b, xs) else dpf2 (b, xs))

