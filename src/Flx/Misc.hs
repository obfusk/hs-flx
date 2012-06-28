--  --                                                            {{{1
--
--  File        : Flx/Misc.hs
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

module Flx.Misc (                                             --  {{{1
  deleteAt, sub
) where                                                       --  }}}1

--

deleteAt :: Int -> [a] -> [a]
deleteAt n _ | n < 0  = error "Flx.Misc.deleteAt: negative index"
deleteAt 0 (_:xt)     = xt
deleteAt n (x:xt)     = x : deleteAt (n - 1) xt
deleteAt _ _          = []

sub :: Integral a => a -> a -> a
sub = subtract

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
