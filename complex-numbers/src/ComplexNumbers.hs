module ComplexNumbers
(Complex,
 conjugate,
 abs,
 real,
 imaginary,
 mul,
 add,
 sub,
 div,
 exp,
 complex) where

import Prelude hiding (div, abs, exp)
import qualified Prelude as Std

-- data definition -----------------------------------------
data Complex a = Complex a a 
                 deriving(Eq, Show)

complex :: (a, a) -> Complex a
complex = uncurry Complex 

-- unary operators -----------------------------------------
conjugate :: Num a => Complex a -> Complex a
conjugate (Complex real imag) = 
    Complex real ( -imag ) 

abs :: Floating a => Complex a -> a
abs (Complex real imag) = 
    sqrt ( real ^ 2 + imag ^ 2 ) 

real :: Num a => Complex a -> a
real (Complex real _) = real 

imaginary :: Num a => Complex a -> a
imaginary (Complex _ imag) = imag 

exp :: Floating a => Complex a -> Complex a
exp (Complex real imag) = 
    Complex (Std.exp real * Std.cos imag) 
            (Std.exp real * Std.sin imag)

-- binary operators ----------------------------------------
mul :: Num a => Complex a -> Complex a -> Complex a
mul (Complex re1 im1) (Complex re2 im2) = 
    Complex (re1 * re2 - im1 * im2) (re1 * im2 + re2 * im1)

add :: Num a => Complex a -> Complex a -> Complex a
add (Complex re1 im1) (Complex re2 im2) = 
    Complex (re1 + re2) (im1 + im2)

sub :: Num a => Complex a -> Complex a -> Complex a
sub (Complex re1 im1) (Complex re2 im2) = 
    Complex (re1 - re2) (im1 - im2)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex re1 im1) (Complex re2 im2) = 
    Complex ((re1 * re2 + im1 * im2) / d) 
            ((im1 * re2 - im2 * re1) / d)
    where 
        d = re2 ^^ 2 + im2 ^^ 2