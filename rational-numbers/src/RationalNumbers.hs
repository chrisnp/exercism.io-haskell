module RationalNumbers
( Rational
, numerator
, denominator
, abs
, add
, sub
, mul
, div
, pow
, expRational
, expReal
, rational) where

import Prelude hiding (div, abs, Rational)

-- Data definition -------------------------------------------------------------
data Rational a = Rational {num, den :: a} deriving(Eq, Show)

rational :: Integral a => (a, a) -> Rational a
rational (n, d) = 
    let 
        hcf = gcd n d
        nr = n `quot` hcf
        dr = d `quot` hcf
        sign = if dr < 0 then -1 else 1 
    in  
        Rational { num = sign * nr, den = sign * dr }

-- unary operators -------------------------------------------------------------
abs :: Integral a => Rational a -> Rational a
abs r = 
    let 
        n = numerator r 
        d = denominator r 
    in 
        rational (n * (signum n), d * (signum d))

numerator :: Integral a => Rational a -> a
numerator r = let Rational n _ = r in n

denominator :: Integral a => Rational a -> a
denominator r = let Rational _ d = r in d

-- binary operators ------------------------------------------------------------
add :: Integral a => Rational a -> Rational a -> Rational a
add r1 r2 =
    let 
        n1 = numerator r1
        d1 = denominator r1
        n2 = numerator r2
        d2 = denominator r2
    in 
        rational (n1 * d2 + n2 * d1, d1 * d2)

sub :: Integral a => Rational a -> Rational a -> Rational a
sub r1 r2 =
    let
        n2 = numerator r2
        d2 = denominator r2
    in
        add r1 (rational ((-n2), d2))

mul :: Integral a => Rational a -> Rational a -> Rational a
mul r1 r2 = 
    let 
        n1 = numerator r1
        d1 = denominator r1
        n2 = numerator r2
        d2 = denominator r2
    in  
        rational (n1 * n2, d1 * d2)

div :: Integral a => Rational a -> Rational a -> Rational a
div r1 r2 =
    let 
        n2 = numerator r2
        d2 = denominator r2
    in  
        mul r1 (rational (d2, n2))

pow :: Integral a => Rational a -> a -> Rational a
pow r e = 
    let 
        n = numerator r
        d = denominator r
    in 
        if e >= 0
        then rational (n ^ e, d ^ e) 
        else rational (d ^ (-e), n ^ (-e))

expRational :: Integral a => Floating b => Rational a -> b -> b
expRational r e = 
    let 
        n = numerator r
        d = denominator r
    in
        (fromIntegral n ** e) / (fromIntegral d ** e)

expReal :: Floating a => Integral b => a -> Rational b -> a
expReal e r = 
    let 
        n = numerator r
        d = denominator r
    in
        e ** (fromIntegral n / fromIntegral d)
