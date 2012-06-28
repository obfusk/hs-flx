--  --                                                            {{{1
--
--  File        : Flx/List/Misc.hs
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

module Flx.List.Misc (                                        --  {{{1
  deleteAt
) where                                                       --  }}}1

--

deleteAt :: Int -> [a] -> [a]
deleteAt n _ | n < 0  = error "Flx.List.Misc.deleteAt: negative index"
deleteAt 0 (_:xt)     = xt
deleteAt n (x:xt)     = x : deleteAt (n - 1) xt
deleteAt _ _          = []

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
