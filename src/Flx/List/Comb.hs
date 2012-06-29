--  --                                                            {{{1
--
--  File        : Flx/List/Comb.hs
--  Maintainer  : Felix C. Stegerman <flx@obfusk.net>
--  Date        : 2012-06-29
--
--  Copyright   : Copyright (C) 2012  Felix C. Stegerman
--  Licence     : GPLv2
--
--  Depends     : ...
--  Description : ...
--
--  TODO        : ...
--
--  Links:
--    http://en.wikipedia.org/wiki/Combinatorics
--
--  --                                                            }}}1

module Flx.List.Comb (                                        --  {{{1
  choose, choose', chooseIf', permute, pairsTup, pairsLst, pairsWith,
  cart
) where                                                       --  }}}1

--

import Flx.List.Misc (deleteAt)

--

choose :: Int -> [a] -> [[a]]
choose k _ | k == 0 = [[]]
choose _ []         = []
choose k (x:xt)     = map (x:) (choose (k-1) xt) ++ choose k xt

choose' :: Int -> [a] -> [[[a]]]
choose' k _ | k == 0  = [[[]]]
choose' _ []          = []
choose' k (x:xt)      = concatMap (map (x:)) (choose' (k-1) xt)
                      : (filter (not . null) $ choose' k xt)

chooseIf' :: ([a] -> Bool) -> Int -> [a] -> [[[a]]]
chooseIf' _ k _ | k == 0  = [[[]]]
chooseIf' _ _ []          = []
chooseIf' p k (x:xt)
  = concatMap (filter p . map (x:)) (chooseIf' p (k-1) xt)
  : (filter (not . null) $ chooseIf' p k xt)

permute :: Int -> [a] -> [[a]]
permute 0 _   = [[]]
permute k xs  = concat
  [ map (x:) . permute (k-1) $ deleteAt i xs | (i,x) <- zip [0..] xs ]

--

pairsTup :: [a] -> [[(a,a)]]
pairsTup = pairsWith (,)

pairsLst :: [a] -> [[[a]]]
pairsLst = pairsWith (\x y -> [x,y])

pairsWith :: (a -> a -> b) -> [a] -> [[b]]
pairsWith f []      = []
pairsWith f [_]     = []
pairsWith f (x:xt)  = map (f x) xt : pairsWith f xt

--

cart :: [[a]] -> [[a]]
cart []       = [[]]
cart (xs:xst) = let ys = cart xst in concat [ map (x:) ys | x <- xs ]

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
