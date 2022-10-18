-- CISC 360 a2, Fall 2019

-- SEE THE FILE a2.pdf
-- for instructions

module A2 where
import Data.Char
import Data.List

-- Q1:
-- Add your student ID:
student_id :: Integer
student_id = 

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE.


{-
  Q2: rewrite

  Hint: Use  mod (ord c) 3  to find the remainder of dividing the ASCII code of c by 3.
-}
rewrite :: String -> String
rewrite []       = []
rewrite (c : cs) 
        | mod (ord c) 3 == 0 = rewrite (delete c cs)
        | mod (ord c) 3 == 1 = [c] ++ rewrite cs
        | mod (ord c) 3 == 2 = replicate 2 c ++ rewrite cs

test1, test2, test3, test4 :: Bool
test1 = rewrite "" == ""
test2 = rewrite "Queen's" == "eeeenns"
test3 = rewrite "Kingston, Ontario" == "nngsttnn,,  Onntta"
test4 = rewrite "Combinatory Logic" == "Cmbbnnatty  Lg"

rewrite_tests :: Bool
rewrite_tests = test1 && test2 && test3 && test4
{-
  Q3: lists
-}

{-
  Q3a. Fill in the definition of listDiff.
  See a2.pdf for instructions.
-}
listDiff :: [Integer] -> [Integer] -> [Bool]
listDiff []     []     = []
listDiff (x:xs) (y:ys) 
        | x == y = [False] ++ listDiff xs ys
        | otherwise = [True] ++ listDiff xs ys
listDiff (x:xs) []     = [True] ++ listDiff xs []
listDiff []     (y:ys) = [True] ++ listDiff [] ys


test_listDiff1 = listDiff [1, 2, 4] [3, 2, 0] == [True, False, True]
test_listDiff2 = listDiff [1, 2, 1, 1] [1, 2] == [False, False, True, True]
test_listDiff3 = listDiff [1, 1] [1, 1, 1, 1] == [False, False, True, True]

{-
  Q3b.
  Briefly explain why listDiff cannot be implemented using the built-in
   function zipWith.

    listDiff :: [Integer] -> [Integer] -> [Bool]
    listDiff = zipWith (/=)

  Write your brief explanation here: 
    zipWith would only work if the 2 lists are of the same length, if one list is 
    shorter than the other it will only return the number of Boolean elements as 
    the length of the shorter list and if one list is empty it will simply return [].
    Therefore, listDiff cannot be implemented using zipWith because the result will not 
    be "padded" with True, so that the result list is as long as the longer input.
-}

{-
  Q3c. Fill in the definition of polyDiff.
  See a2.pdf for instructions.
-}
polyDiff :: (a -> a -> Bool) -> [a] -> [a] -> [Bool]
polyDiff ne []     []     = []
polyDiff ne (x:xs) (y:ys) = [ne x y] ++ polyDiff ne xs ys
polyDiff ne (x:xs) []     = [True] ++ polyDiff ne xs []
polyDiff ne []     (y:ys) = [True] ++ polyDiff ne [] ys

test_polyDiff1 = polyDiff (\i -> \j -> i /= j) [1, 2, 4] [3, 2, 0] == [True, False, True]

-- whoever calls polyDiff gets to define what "not equal" means:
--  in test_polyDiff2, the definition of "not equal" becomes whether two lists (here, strings) have different lengths
lengthsDifferent :: [a] -> [a] -> Bool
lengthsDifferent xs ys = (length xs /= length ys)
test_polyDiff2 = polyDiff lengthsDifferent ["a", "ab", "abcd"] ["ccc", "xy", ""] == [True, False, True]


{-
   Q4: Super Smash Spinning

   ***See a2.pdf for instructions*** and some (possibly) helpful information.
-}

data Disc = Smash Disc Disc
          | Lean
          | Spin
          deriving (Show, Eq)

{-
  dj: Perform a move described in a2.pdf
-}
dj :: Disc -> Disc

-- The Smash-and-Lean
dj (Smash (Smash Lean middle) right) = middle

-- The Super Smash
dj (Smash (Smash (Smash Spin f) r1) r2) = Smash (Smash f r2) (Smash r1 r2)

-- All other patterns: just return the argument
dj other = other

{-
  Q4a. Stepping dj

f = lean
r1 = Lean
r2 = Spin

Q4a Example:

    dj (Smash (Smash (Smash Spin Lean) Lean)) Spin
=>  Smash (Smash Lean Spin) (Smash Lean Spin)     by function application rule
                                                  with substitution:
                                                   Lean for f,
                                                    Lean for r1,
                                                    Spin for r2
Q4a.1:

    dj (Smash (Smash Lean Spin) (Smash Lean Spin))
=>  Spin                                            by function application rule
                                                     with substitution [
                                                       Spin for middle,
                                                       (Smash Lean Spin) for right
                                                     ] 
Q4a.2:

    dj (Smash (Smash Spin Lean) Spin)
=>  (Smash (Smash Spin Lean) Spin)                  by function application rule
                                                     with substitution [
                                                       (Smash (Smash Spin Lean) Spin) for other
                                                     ] 
-}


{-
   Q4b. revTree
   ***See a2.pdf for instructions.***
   Note that you need to write your own type declaration and test cases,
   in addition to defining revTree.
-}

revTree :: Disc -> Disc

revTree (Smash leftChild rightChild) = Smash (revTree(rightChild)) (revTree(leftChild))
revTree Lean = Lean
revTree Spin = Spin

testRev1, testRev2 :: Bool
testRev1 = revTree (Smash Lean Spin) == Smash Spin Lean
testRev2 = revTree (Smash (Smash Lean Spin) (Lean)) == Smash Lean (Smash Spin Lean)


{-
   Q4c. dance
   ***See a2.pdf for instructions.***
-}

dance :: Disc -> Disc
dance disc 
      | dj disc == disc = disc 
      | dj disc /= disc = dance (dj disc)

{-
  testDance1 and testDance3 will stop at the first guard as they only match other
  testDance2: ((Smash (Smash (Smash Spin Lean) Lean)) Spin)   by func app
              Smash (Smash Lean Spin) (Smash Lean Spin)       with substitution [
              Spin                                               Lean for f, Lean for r1, 
                                                                Spin for r2, Spin for middle
                                                              ]              
-}

testDance1, testDance2, testDance3 :: Bool
testDance1 = dance Spin == Spin
testDance2 = dance ((Smash (Smash (Smash Spin Lean) Lean)) Spin) == Spin
testDance3 = dance (Smash (Smash Spin Lean) Spin) == Smash (Smash Spin Lean) Spin

{-
  Q5.1: Bonus question.
  SEE a2.pdf FOR INSTRUCTIONS.
  Do NOT attempt this bonus question until you are completely satisfied
  with your work on the questions above.
-}

discplayer :: Disc -> String
discplayer Lean = "(\\x -> (\\y -> x))"
discplayer Spin = "(\\a -> (\\b -> (\\c -> a c (b c))))"
discplayer (Smash d1 d2) = "(" ++ (discplayer d1) ++ " " ++ (discplayer d2) ++ ")"

cleanplayer = (\d -> putStrLn (discplayer d))

{-
  Add or remove blanks as needed.

1. The Disc sc is Spin

2. The Disc kc is Lean

3. The Disc i is _________________________________________________

4. The Disc j is _________________________________________________

-}
