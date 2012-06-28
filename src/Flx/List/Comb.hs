--  --                                                            {{{1
--
--  File        : Flx/List/Comb.hs
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
--  Links:
--    http://en.wikipedia.org/wiki/Combinatorics
--
--  --                                                            }}}1

module Flx.List.Comb (                                        --  {{{1
  choose, permute
) where                                                       --  }}}1

--

import Flx.List.Misc (deleteAt)

--

choose :: Int -> [a] -> [[a]]
choose k _ | k == 0 = [[]]
choose _ []         = []
choose k (x:xt)     = map (x:) (choose (k-1) xt) ++ choose k xt

permute :: Int -> [a] -> [[a]]
permute 0 _   = [[]]
permute k xs  = concat
  [ map (x:) . permute (k-1) $ deleteAt i xs | (i,x) <- zip [0..] xs ]

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
