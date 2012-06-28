--  --                                                            {{{1
--
--  File        : Flx/Euler.hs
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

module Flx.Euler (                                            --  {{{1
  divisors, isPrimeMemo, isPalindrome
) where                                                       --  }}}1

--

import Data.Function.Memoize (memoize)
import Data.List (nub, subsequences)
import Data.Numbers.Primes (isPrime, primeFactors)

import Flx.List.Misc (product')

--

divisors :: Integral a => a -> [a]
divisors = nub . map product' . init . subsequences . primeFactors

isPrimeMemo :: Int -> Bool
isPrimeMemo = memoize isPrime

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs
  = let n = length xs `div` 2 in take n xs == (take n $ reverse xs)

-- vim: set tw=70 sw=2 sts=2 et fdm=marker :
