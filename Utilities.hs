module Utilities where

{- map2: Tuple mapper
Given: tuple of functions and a tuple of values
Returns: tuple of function applied on values
-}
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map2 (f1, f2) (x1, x2) = (f1 x1, f2 x2)
{- Mayber mapper
Given: function and a Maybe
Returns: function mapped on Maybe
-}
mmap :: (a -> b) -> Maybe a -> Maybe b
mmap f  Nothing  = Nothing
mmap f (Just x)  = Just (f x)
{-
or clause
-}
orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x  = x
orElse (Just a) _  = Just a
{-
try clause: returns f(x), or x if nothing
-}
try :: (a -> Maybe a) -> a -> a
try f x = maybe x id (f x)
{-
checks if f(x) = x else
	recurse on fix f f(x)
-}
fix :: Eq a => (a -> a) -> a -> a
fix f x
   |  f x == x  = x
   |  otherwise = fix f (f x)



{-
list[floor(u*Int(len(xs)))]
-}
pick :: RealFrac r => r -> [a] -> a
pick u xs = xs !! (floor.(u*).fromIntegral.length) xs

