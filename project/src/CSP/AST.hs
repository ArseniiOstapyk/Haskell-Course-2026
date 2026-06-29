module CSP.AST
  ( Program(..)
  , VarDecl(..)
  , Domain(..)
  , Value(..)
  , Constraint(..)
  , BinOp(..)
  , NAryOp(..)
  , Assignment
  , domainValues
  , prettyAssignment
  ) where

import qualified Data.Map.Strict as M
import Data.List (intercalate)

-- | A complete CSP program: variable declarations plus constraints.
data Program = Program [VarDecl] [Constraint]
  deriving (Eq, Show)

-- | One declaration may be expanded by the parser from syntax like:
--   var A, B, C : { red, green, blue };
data VarDecl = VarDecl String Domain
  deriving (Eq, Show)

data Domain
  = IntRange Int Int        -- ^ Inclusive range, e.g. 1..9
  | DiscreteSet [Value]     -- ^ Explicit finite set, e.g. { red, green }
  deriving (Eq, Show)

data Value
  = IntVal Int
  | StrVal String
  | BoolVal Bool
  deriving (Eq, Ord)

instance Show Value where
  show (IntVal n) = show n
  show (StrVal s) = s
  show (BoolVal b) = if b then "true" else "false"

data Constraint
  = Binary BinOp String String       -- ^ x /= y, x <= y, ...
  | NAry NAryOp [String]             -- ^ allDifferent [x, y, z]
  deriving (Eq, Show)

data BinOp = Eq | NEq | Lt | Le | Gt | Ge
  deriving (Eq, Show)

data NAryOp = AllDifferent
  deriving (Eq, Show)

type Assignment = M.Map String Value

domainValues :: Domain -> Either String [Value]
domainValues (DiscreteSet xs) = Right xs
domainValues (IntRange lo hi)
  | lo <= hi   = Right [IntVal n | n <- [lo .. hi]]
  | otherwise  = Left $ "empty integer range " ++ show lo ++ ".." ++ show hi

prettyAssignment :: Assignment -> String
prettyAssignment asg =
  let pairs = [name ++ " = " ++ show value | (name, value) <- M.toAscList asg]
   in "{ " ++ intercalate ", " pairs ++ " }"
