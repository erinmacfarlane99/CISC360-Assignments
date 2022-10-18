-- CISC 360 a1, Fall 2019

module A1 where

-- Q1:
-- Add your student ID:
student_id :: Integer
student_id = 

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE.


-- Q2.1: between

-- between m n p == True if and only if n is between m and p
-- (not *strictly* between:  between 1 1 4 and between 10 300 300 should return True)
--
between :: Integer -> Integer -> Integer -> Bool
between m n p = if (m <= n) && (n <= p) then True else False

-- Testing between:
--
-- Test cases for between
test_between1, test_between2, test_between3, test_between4, test_between5 :: Bool
test_between1 = between 1 2 3
test_between2 = not (between 1 0 5)
test_between3 = between (-2) (-2) (-1)
test_between4 = between 1 2 2
test_between5 = not (between 0 30 29)
-- Do all 5 tests together
test_between :: Bool
test_between  = test_between1 && test_between2
                              && test_between3
                              && test_between4
                              && test_between5

{-
   Done with between?  Move on to the next function.
-}

-- Q2.2: pin
--
-- pin m n p  returns  m  if  n < m,
--                     n  if  m <= n <= p,
--                     p  if  n > p
-- (it "pins" n to the interval between m and p)
-- Note: This specification does not clearly say what to do if m > p.
--       So you can do *anything* in that situation: return m, return undefined, ...
pin :: Integer -> Integer -> Integer -> Integer
pin m n p = 
    if (m > p) then error "lower bound greater than upper bound"
    else if (n < m) then m 
        else if (between m n p) then n 
            else p
               

-- Testing pin:
--
test_pin1 = (pin 1 3 5) == 3
test_pin2 = (pin 2 1 6) == 2
test_pin3 = (pin 2 45 42) == 42
test_pin = test_pin1 && test_pin2 && test_pin3

{-
Stepping questions

Q3.1. Replace the underlines (_______).
 
   expression                            justification

   (\x -> 3 * x * x) 2             
=> 3 * 2 * 2                             by substitution
=> 6 * 2                                 by arithmetic
=> 12                                    by arithmetic


Q3.2.  Replace the underlines (_______).
   Assume a function double has been defined:

   double :: Integer -> Integer
   double x = 2 * x

     expression                            justification

     (\a -> \b -> a (a b)) double 4
  => double (double b)                     by function application
  => double (double 4)                     by function application
  => double (2 * 4)                        by function application
  => double (8)                            by function applicatiom 
  => 2 * 8                                 by arithmetic
  => 16                                    by arithmetic
-}

{-
Q4.

  The following function is named "valencia".
  Given two natural numbers k and n,

                      n
                     ___  | n + i |
    valencia k n  =  | |  | ----- |
                     | |  |_  i  _|

                     i=k

  If k > n, valencia k n = 1.

  This "big-Pi" notation means
  "take the product of the thing on the right for all i from k to n",
  where "the thing on the right" is
 
     | n + i |
     | ----- |
     |_  i  _|

  The |_ _| represents taking the "floor": rounding down to an integer.
  Haskell has a built-in function called  div  that divides *and* takes the floor.
  For example,
                 |        |   |      |
     div 19 4  = | 19 / 4 | = | 4.75 | = 4
                 |_      _|   |_    _|
-}

valencia :: Integer -> Integer -> Integer
valencia k n = if k > n then 1
               else div (n + k) k * valencia (k + 1) n

-- Testing valencia:
test_valencia1, test_valencia2, test_valencia3, test_valencia4 :: Bool
test_valencia1 = (valencia 6 5  == 1)
test_valencia2 = (valencia 5 5  == 2)

test_valencia3 = (valencia 4 7  == div (7+4) 4 * div (7+5) 5 * div (7+6) 6 * div (7+7) 7)

test_valencia4 = (valencia 3 16 == 829440)

test_valencia  = test_valencia1 && test_valencia2
                                && test_valencia3
                                && test_valencia4

-- Q5.1: toBinary
--
-- toBinary n  ==  natural number `n' converted to a string in base 2
--
-- For example, 16 in binary is 10000, so
--
--   toBinary 16 == "10000"
--
-- Hints:
--    1. The built-in function  show  converts an integer to a string *in decimal*.
--         It may or may not be useful here, and/or in the function toNary.
--
--    2. You can use the built-in function  ++  to concatenate strings.
--         For example, "10" ++ "0" == "100".
--
--    3. Use the div and mod functions.  div was described above.
--         mod divides and returns the remainder; for example, mod 65 2 == 1,
--         because the remainder of 65 divided by 2 is 1.

toBinary :: Integer -> [Char]
toBinary n 
      | n == 0 = "0"
      | n == 1 = "1"
      | mod n 2 == 1 = toBinary(div (n - 1) 2) ++ "1"      
      | otherwise = toBinary(div n 2) ++ "0"


test_toBinary1 = (toBinary 0 == "0")
test_toBinary2 = (toBinary 1 == "1")
test_toBinary3 = (toBinary 16 == "10000")
test_toBinary4 = (toBinary 32 == "100000")
test_toBinary5 = (toBinary 64 == "1000000")
test_toBinary6 = (toBinary 65 == "1000001")
test_toBinary = test_toBinary1 && test_toBinary2
                               && test_toBinary3
                               && test_toBinary4
                               && test_toBinary5
                               && test_toBinary6

-- Q5.2: toNary
--
-- toNary base n  ==  `n' converted to a string in base `base'
--
-- Raises an error if `base' is less than 2 or greater than 10.
--
-- When `base' is 2, toNary should give the same result as toBinary.
-- That is, for all `n', (toNary 2 n) == (toBinary n).
-- 


toNary :: Integer -> Integer -> [Char]
toNary base n
     | base < 2 || base > 10 = error "invalid base"
     | n == 0 = []
     | otherwise = toNary base (div n base) ++ show (mod n base)

test_toNary6 = (toNary 2 65 == "1000001")
test_toNaryOctal = (toNary 8 2880249322 == "25353216752")
--                           0xabad1dea











