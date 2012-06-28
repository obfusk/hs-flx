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
  deleteAt, sum', product'
) where                                                       --  }}}1

--

import Data.List (foldl')

--

deleteAt :: Int -> [a] -> [a]
deleteAt n _ | n < 0  = error "Flx.List.Misc.deleteAt: negative index"
deleteAt 0 (_:xt)     = xt
deleteAt n (x:xt)     = x : deleteAt (n - 1) xt
deleteAt _ _          = []

sum', product' :: Num a => [a] -> a
sum'      = foldl' (+) 0
product'  = foldl' (*) 1

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
