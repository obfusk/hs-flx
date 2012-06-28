--  --                                                            {{{1
--
--  File        : Flx/TODO.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2012-06-28
--
--  Copyright   : Copyright (C) 2012  Felix C. Stegerman
--  Licence     : GPLv2
--
--  Depends     : ...
--  Description : ...
--
--  TODO        : ...
--
--  --                                                            }}}1

module Flx.TODO (                                             --  {{{1
  cart, diagCart, diagChoose, pairs, pairsWith
) where                                                       --  }}}1

--

import Data.List.Ordered (insertSet, mergeAllBy, unionAllBy)

--

cart :: [[a]] -> [[a]]
cart []       = error "cart: empty list"
cart [xs]     = map (:[]) xs
cart (xs:xss) = let ys = cart xss in concat [ map (x:) ys | x <- xs ]

diagCart :: ([a] -> [a] -> Ordering) -> [[a]] -> [[a]]
diagCart _ []   = error "diagCart: empty list"
diagCart _ [xs] = map (:[]) xs
diagCart f (xs:xss)
  = let ys = diagCart f xss in mergeAllBy f [ map (x:) ys | x <- xs ]

diagChoose :: (Ord a, Ord b) => ([a] -> b) -> Int -> [a] -> [[a]]
diagChoose _ 1 xs = map (:[]) xs
diagChoose f n xs
    = let ys = diagChoose f (n - 1) xs
      in  unionAllBy (\x y -> compare (f x,x) (f y,y))
          $ map (filter ((== n) . length))
          [ map (insertSet x) ys | x <- xs ]

pairs :: [a] -> [[(a,a)]]
pairs = pairsWith (,)

pairsWith :: (a -> a -> b) -> [a] -> [[b]]
pairsWith f []      = []
pairsWith f [_]     = []
pairsWith f (x:xt)  = map (f x) xt : pairsWith f xt

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
