{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Typeclasses where

class TC a where
   method1 :: a -> String
   method2 :: String -> a

instance TC Int where
   method1 _ = "Int"
   method2 _ = 0

instance TC String where
   method1 x = "String: " ++ x
   method2 x = x

f :: (TC a) => a -> Char
f = head . method1

h :: Int -> Char
h = f
