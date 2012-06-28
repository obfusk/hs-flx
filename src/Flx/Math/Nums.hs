--  --                                                            {{{1
--
--  File        : Flx/Math/Nums.hs
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
--    http://en.wikipedia.org/wiki/Polygonal_number
--
--  --                                                            }}}1

module Flx.Math.Nums (                                        --  {{{1
  fibs, nat, polygonal, fromPolygonal, polygonals
) where                                                       --  }}}1

--

import Data.Maybe.HT (toMaybe)

--

fibs :: Integral a => [a]
fibs = 1 : scanl (+) 1 fibs

--

nat :: (Integral a,RealFrac b) => (b -> b) -> (a -> a) -> a -> Maybe a
nat f g x = let y = round . f $ fromIntegral x in toMaybe (x == g y) y

--

polygonal :: Integral a => a -> a -> a
polygonal s n = ((s - 2)*n^2 - (s - 4)*n) `div` 2

fromPolygonal :: Integral a => a -> a -> Maybe a
fromPolygonal s
  = let h x = (8*s - 16)*x + (s - 4)^2
        f x = (sqrt x + s' - 4) / (2*s' - 4)
        g x = (x * (2*s - 4) - s + 4)^2
        s'  = fromIntegral s
    in  nat f g . h

polygonals :: Integral a => a -> [a]
polygonals s
  | s >= 2    = scanl (+) 1 [(s-1),(s*2 - 3)..]
  | otherwise = error "Flx.Math.Nums.polygonals: s must be >= 2"

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
