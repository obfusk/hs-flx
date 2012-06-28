--  --                                                            {{{1
--
--  File        : Flx/Math/Comb.hs
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

module Flx.Math.Comb (                                        --  {{{1
  fac, facs, choices
) where                                                       --  }}}1

--

import Data.List (genericIndex)

--

fac :: Integral a => a -> a
fac = genericIndex facs

facs :: Integral a => [a]
facs = scanl (*) 1 [1..]

choices :: Integral a => a -> a -> a
choices n r = (fac n) `div` (fac r * fac (n - r))

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
