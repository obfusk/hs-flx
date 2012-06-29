--  --                                                            {{{1
--
--  File        : Flx/Misc.hs
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
--  --                                                            }}}1

module Flx.Misc (                                             --  {{{1
  (-!), swap, fst3, snd3, trd3, fst4, snd4, trd4, fth4
) where                                                       --  }}}1

--

(-!) :: Num a => a -> a -> a
(-!) = (-)

infixl 6 -!

--

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

fst3 :: (a,b,c) -> a
snd3 :: (a,b,c) -> b
trd3 :: (a,b,c) -> c

fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

fst4 :: (a,b,c,d) -> a
snd4 :: (a,b,c,d) -> b
trd4 :: (a,b,c,d) -> c
fth4 :: (a,b,c,d) -> d

fst4 (a,_,_,_) = a
snd4 (_,b,_,_) = b
trd4 (_,_,c,_) = c
fth4 (_,_,_,d) = d

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
