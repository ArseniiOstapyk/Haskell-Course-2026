module CSP.Solver
  ( solveOne
  , solveAll
  , checkConstraint
  , satisfiesAll
  ) where

import CSP.AST
import qualified Data.Map.Strict as M
import Data.List (nub, sortOn)
import Data.Maybe (mapMaybe)

type Domains = M.Map String [Value]

solveOne :: Program -> Either String (Maybe Assignment)
solveOne p = do
  sols <- solveN 1 p
  pure $ case sols of
    [] -> Nothing
    x:_ -> Just x

solveAll :: Program -> Either String [Assignment]
solveAll = solveN maxBound

solveN :: Int -> Program -> Either String [Assignment]
solveN limit (Program decls constraints) = do
  domains <- buildDomains decls
  validate constraints domains
  pure (take limit (search constraints domains M.empty))

buildDomains :: [VarDecl] -> Either String Domains
buildDomains = foldr add (Right M.empty)
  where
    add (VarDecl name dom) acc = do
      m <- acc
      vals <- domainValues dom
      if null vals
        then Left $ "variable " ++ name ++ " has an empty domain"
        else case M.lookup name m of
          Just _ -> Left $ "variable declared more than once: " ++ name
          Nothing -> Right (M.insert name (nub vals) m)

validate :: [Constraint] -> Domains -> Either String ()
validate constraints domains =
  let names = M.keys domains
      mentioned = concatMap constraintVars constraints
      unknown = filter (`notElem` names) mentioned
   in if null unknown
        then Right ()
        else Left $ "unknown variable(s) in constraint(s): " ++ show (nub unknown)

constraintVars :: Constraint -> [String]
constraintVars (Binary _ a b) = [a, b]
constraintVars (NAry _ xs) = xs

search :: [Constraint] -> Domains -> Assignment -> [Assignment]
search constraints domains asg
  | M.size asg == M.size domains =
      [asg | satisfiesAll constraints asg]
  | otherwise =
      case chooseVariable constraints domains asg of
        Nothing -> []
        Just (name, values) -> do
          value <- values
          let asg' = M.insert name value asg
          guardList (partialOK constraints asg')
          search constraints domains asg'

chooseVariable :: [Constraint] -> Domains -> Assignment -> Maybe (String, [Value])
chooseVariable constraints domains asg =
  case candidates of
    [] -> Nothing
    xs -> Just (head (sortOn (length . snd) xs))
  where
    unassigned = [(name, vals) | (name, vals) <- M.toList domains, M.notMember name asg]
    viable name vals = [v | v <- vals, partialOK constraints (M.insert name v asg)]
    candidates = [(name, viable name vals) | (name, vals) <- unassigned]

partialOK :: [Constraint] -> Assignment -> Bool
partialOK constraints asg = all (checkConstraintPartial asg) constraints

satisfiesAll :: [Constraint] -> Assignment -> Bool
satisfiesAll constraints asg = all (== Just True) (map (checkConstraint asg) constraints)

-- | Check a constraint under a complete assignment.
--   Returns Nothing if a referenced variable is missing.
checkConstraint :: Assignment -> Constraint -> Maybe Bool
checkConstraint asg (Binary op a b) = do
  av <- M.lookup a asg
  bv <- M.lookup b asg
  pure (applyBinOp op av bv)
checkConstraint asg (NAry AllDifferent xs) = do
  vals <- traverse (`M.lookup` asg) xs
  pure (length vals == length (nub vals))

-- | Partial check for pruning. Unknown pieces are not failures yet, except
--   allDifferent can fail early as soon as two assigned variables are equal.
checkConstraintPartial :: Assignment -> Constraint -> Bool
checkConstraintPartial asg c@(Binary _ a b) =
  case (M.lookup a asg, M.lookup b asg) of
    (Just _, Just _) -> checkConstraint asg c == Just True
    _ -> True
checkConstraintPartial asg (NAry AllDifferent xs) =
  let assigned = mapMaybe (`M.lookup` asg) xs
   in length assigned == length (nub assigned)

applyBinOp :: BinOp -> Value -> Value -> Bool
applyBinOp Eq  a b = a == b
applyBinOp NEq a b = a /= b
applyBinOp Lt  a b = comparable (<)  a b
applyBinOp Le  a b = comparable (<=) a b
applyBinOp Gt  a b = comparable (>)  a b
applyBinOp Ge  a b = comparable (>=) a b

comparable :: (Int -> Int -> Bool) -> Value -> Value -> Bool
comparable f (IntVal a) (IntVal b) = f a b
comparable _ _ _ = False

guardList :: Bool -> [()]
guardList True = [()]
guardList False = []
