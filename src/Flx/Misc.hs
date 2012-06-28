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
  sub
) where                                                       --  }}}1

--

sub :: Integral a => a -> a -> a
sub = subtract

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
