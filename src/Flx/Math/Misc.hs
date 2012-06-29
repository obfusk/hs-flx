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
  sqrtContFrac
) where                                                       --  }}}1

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

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
