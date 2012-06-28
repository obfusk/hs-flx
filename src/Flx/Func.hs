--  --                                                            {{{1
--
--  File        : Flx/Func.hs
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

module Flx.Func (                                             --  {{{1
  cps, cps2, cps3, cps4, cps5
) where                                                       --  }}}1

--

cps   :: (b -> c) -> (a -> b)
                  ->  a -> c
cps2  :: (c -> d) -> (a -> b -> c)
                  ->  a -> b -> d
cps3  :: (d -> e) -> (a -> b -> c -> d)
                  ->  a -> b -> c -> e
cps4  :: (e -> f) -> (a -> b -> c -> d -> e)
                  ->  a -> b -> c -> d -> f
cps5  :: (f -> g) -> (a -> b -> c -> d -> e -> f)
                  ->  a -> b -> c -> d -> e -> g

cps                 = (.)
cps2 f g a b        = f $ g a b
cps3 f g a b c      = f $ g a b c
cps4 f g a b c d    = f $ g a b c d
cps5 f g a b c d e  = f $ g a b c d e

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
