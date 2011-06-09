{-
 -      ``Scratch''
 -      (c) 2009 James Cook
 -}
{-# LANGUAGE
    GADTs,
    FlexibleContexts,
    FlexibleInstances,
    UndecidableInstances,
    RankNTypes,
    MultiParamTypeClasses
  #-}

module Scratch where

import Control.Monad
import Data.Random

data GT s a =
    S (s (GT s) a)
    | Const a

data S x c where
    Op :: String -> (a -> b -> x c) -> x a -> x b -> S x c

class Block s e a where
    inject :: s e a -> e a

instance Block s (GT s) a where
    inject = S

instance Functor (s (GT s)) => Functor (GT s) where
    fmap f (Const x) = Const (f x)
    fmap f (S x) = S (fmap f x)

instance Monad (GT S) where
    return = Const
    (Const x) >>= f = f x
    (S x) >>= f = case seqS (fmap f x) of
        Const x -> S x
        S x     -> undefined

newtype C f g x = C { unC :: f (g x) }

instance Functor x => Functor (S x) where
    fmap f (Op name (*) a b) = Op name (\x y -> fmap f (x * y)) a b

 -- fooS :: (forall t. y t -> x t) -> S y a -> S x a
 -- fooS f (Op name op a b) = Op name op (f a) (f b)

seqS :: Monad x => S x (x a) -> x (S x a)
seqS (Op name op a b) = return (Op name (\x y -> join (op x y)) a b)

type GTS a = GT S a