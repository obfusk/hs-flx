--  --                                                            {{{1
--
--  File        : Flx/Math/Misc.hs
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
--    http://en.wikipedia.org/wiki/Methods_of_computing_square_roots
--      #Continued_fraction_expansion
--
--  --                                                            }}}1

module Flx.Math.Misc (                                        --  {{{1
  sqrtContFrac, sqrtContConv, convergents, convergents'
) where                                                       --  }}}1

--

import Data.List (zipWith4)

--

sqrtContFrac :: Integral a => a -> [a]
sqrtContFrac s
    = map (\(_,_,a) -> a) $ iterate next (0,1,a0)
  where
    next (m,d,a) = (m',d',a')
      where
        m'  = d*a - m
        d'  = (s - m'^2) `div` d
        a'  = (a0 + m') `div` d'
    a0 = floor . sqrt $ fromIntegral s

sqrtContConv :: Integral a => a -> [(a,a)]
sqrtContConv = convergents' . sqrtContFrac

convergents :: Integral a => [a] -> [a] -> [(a,a)]
convergents (a1:at) (b0:b1:bt)
    = cs
  where
    cs = (x0,y0) : (x1,y1) : zipWith4 next cs (tail cs) at bt
    x0 = b0         ; y0 = 1
    x1 = b1*b0 + a1 ; y1 = b1
    next (x,y) (x',y') a b = (b*x' + a*x,b*y' + a*y)

convergents' :: Integral a => [a] -> [(a,a)]
convergents' = convergents (repeat 1)

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
