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
  (-!)
) where                                                       --  }}}1

--

(-!) :: Num a => a -> a -> a
(-!) = (-)

infixl 6 -!

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
