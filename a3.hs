-- CISC 360 a3, Fall 2019

-- SEE THE FILE a3.pdf
-- for instructions

module A3
where
import Data.List

-- Q1:
-- Add your student ID:
student_id :: Integer
student_id = 

-- THIS FILE WILL NOT COMPILE UNTIL YOU ADD YOUR STUDENT ID ABOVE.

{-
Q2: Truth Tables

In order to build a truth table for a formula, there are 4 steps:

1) Traverse the formula to find all atomic propositions (propositional variables).

2) Assign all possible combinations of True and False
   to the set of atomic propositions in the formula.

3) Evaluate the formula for each valuation obtained in (2).

4) Use the results of (1-3) to build the table.

In this question, you will implement steps (1-3).
-}

-- Variable is a synonym for String.
type Variable = String

-- In our simplified version of classical propositional logic,
-- we have the following definition for a Formula:
data Formula = TOP                          -- truth
             | BOT                          -- falsehood
             | AND Formula Formula          -- conjunction
             | IMPLIES Formula Formula      -- implication
             | VAR Variable                 -- atomic proposition (propositional variable)
             deriving (Eq, Show)

-- Some propositional variables, for convenience
vA = VAR "A"
vB = VAR "B"
vC = VAR "C"
vD = VAR "D"
vE = VAR "E"
vF = VAR "F"

-- Some example formulas that you can use to test your functions
formula1 = IMPLIES (AND vA vB) vC
formula2 = IMPLIES BOT (AND vA vB)
formula3 = IMPLIES (AND vA vB) TOP
formula4 = AND (IMPLIES vA (AND vB vC)) (AND vD vE)
formula5 = AND vA vB
formula6 = IMPLIES vA vB

-- A Valuation is a list of pairs corresponding to a truth value (i.e. True or False) for each Variable in a formula
type Valuation = [(Variable, Bool)]

-- A TruthTable is an enumeration of the valuations for a given formula,
-- with each valuation paired with the corresponding evaluation of that formula.
-- (This corresponds to a truth table with no "intermediate columns".)
data TruthTable = TruthTable [(Valuation, Bool)]

-- This function is here so that when you print a TruthTable in GHCi, the table is nice and readable.
-- You don't need to understand how this works to complete the assignment.
instance Show TruthTable where
  show (TruthTable rows) =
    case rows of
      [] -> ""
      ([], result) : _ -> "   result is " ++ pad_show result ++ "\n"
      ((c,b) : valu, result) : xs -> 
        c ++ "=" ++ (pad_show b) ++ "   "
          ++ show (TruthTable [(valu,result)])
          ++ show (TruthTable xs)
    where
      pad_show True  = "True "
      pad_show False = "False"

{- Q2a: getVars:
  Traverse a formula and build a list of all VARs in the formula, without duplicates.
  Applied to a Formula and an (initially empty) list.
  The list parameter can be thought of as an accumulator to build up the list of VARs.
  Note: it may be convenient to use a built-in function called "nub",
  which removes all duplicates from a list.  Why is it called nub?  I have no idea.
-}

getVars :: Formula -> [Variable] -> [Variable]
getVars TOP               vList = vList
getVars BOT               vList = vList
getVars (VAR v)           vList = [v]
getVars (AND phi1 phi2)   vList = nub (getVars phi1 vList ++ getVars phi2 vList)
getVars (IMPLIES phi psi) vList = nub (getVars phi vList ++ getVars psi vList)

-- Q2b: getValuations:
--  Build a list of all possible valuations for a set of variables
getValuations :: [Variable] -> [Valuation]
getValuations []       = [[]]
getValuations (c : cs) = map (\xs -> (c, True) : xs) (getValuations cs) ++ map (\xs -> (c, False) : xs) (getValuations cs)

-- Q2c: evalF:
--  Evaluate a formula with a particular valuation,
--   returning the resulting boolean value
searchVal :: Variable -> Valuation -> Bool 
searchVal v (y:ys) = if v == fst y then snd y else searchVal v ys

evalF :: Formula -> Valuation -> Bool
evalF TOP             _    = True
evalF BOT             _    = False
evalF (VAR c)         valu = searchVal c valu
evalF (AND q1 q2)     valu = evalF q1 valu && evalF q2 valu
evalF (IMPLIES q1 q2) valu = (evalF q1 valu) == False || (evalF q2 valu) 
-- buildTable:
--  Build a truth table for a given formula.
--  You can use this function to help check your definitions
--  of getVars, getValuations and evalF.
buildTable :: Formula -> TruthTable
buildTable psi =
  let valuations = getValuations (getVars psi [])
  in
    TruthTable (zip valuations
                    (map (evalF psi) valuations))

{-
Q3: Tiny Theorem Prover
-}

-- a Context is a list of Formulas, representing assumptions
type Context = [Formula]

-- prove ctx phi:
--   return True if, assuming everything in ctx is true,
--    the formula phi is true according to the rules given in a3.pdf.
--   otherwise, return False.
prove :: Context -> Formula -> Bool
prove ctx phi = ((elem phi (decompose ctx [])) || (elem BOT (decompose ctx [])))  && prove_right ctx phi
  -- Follow the description in a3.pdf ("Your function `prove' should work like this:")

-- decompose ctx1 ctx2
--  move through ctx2, decomposing ANDs into standalone assumptions
--                     and eliminating IMPLIESes where possible
--                     (see a3.pdf).
-- invariants:
--  - ctx1 is completely decomposed (no formula in ctx1 is (AND _ _))
--  - ctx2 is a "queue" of assumptions to decompose
decompose :: Context -> Context -> Context
decompose ctx1 []              = ctx1
decompose ctx1 (middle : ctx2) =
  case middle of
    AND phi1 phi2   -> undefined
    -- (phi1 : ctx1) && (phi2 : ctx1)
    IMPLIES phi psi -> undefined
    middle          -> decompose (ctx1 ++ [middle]) ctx2

-- prove_right:
--  assuming the context is decomposed,
--  apply -Right rules (see a3.pdf)
--   to break down the goal formula
--  ("right" because we are working on the formula on the right-hand side,
--   after the assumptions)
prove_right :: Context -> Formula -> Bool

prove_right ctx TOP               = True     -- TOP-Right

prove_right ctx (AND phi1 phi2)   = (prove ctx phi1) && (prove ctx phi2)
  -- try to apply AND-Right

prove_right ctx (IMPLIES phi psi) = (prove (phi : ctx) psi)
  -- try to apply IMPLIES-Right
  
prove_right ctx p             = False
  -- couldn't apply any of the -Right rules, so give up

test_imp1 = prove [IMPLIES vB vC] (IMPLIES vB vC)
test_imp2 = prove [IMPLIES vB vC] (IMPLIES (AND vB vB) vC)
test_imp3 = not (prove [IMPLIES (AND vB vD) vC] (IMPLIES vB vC))
