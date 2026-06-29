module Main where

import CSP.AST
import CSP.Parser
import CSP.Solver
import qualified Data.Map.Strict as M
import System.Exit (exitFailure)

main :: IO ()
main = do
  results <- sequence
    [ testParserMapColouring
    , testBinaryConstraint
    , testAllDifferentPartial
    , testMapColouringSolves
    , testUnsat
    , testTinyQueens
    ]
  if and results
    then putStrLn "All tests passed."
    else exitFailure

assert :: String -> Bool -> IO Bool
assert label ok = do
  putStrLn $ (if ok then "PASS " else "FAIL ") ++ label
  pure ok

testParserMapColouring :: IO Bool
testParserMapColouring = case parseProgram australia of
  Right (Program decls cons) -> assert "parser expands declarations and constraints" (length decls == 7 && length cons == 9)
  Left err -> assert ("parser failed: " ++ renderParseError err) False

testBinaryConstraint :: IO Bool
testBinaryConstraint = do
  let asg = M.fromList [("x", IntVal 2), ("y", IntVal 3)]
  assert "binary constraints" $ and
    [ checkConstraint asg (Binary NEq "x" "y") == Just True
    , checkConstraint asg (Binary Lt "x" "y") == Just True
    , checkConstraint asg (Binary Eq "x" "y") == Just False
    ]

testAllDifferentPartial :: IO Bool
testAllDifferentPartial = do
  let p = Program [VarDecl "a" (IntRange 1 1), VarDecl "b" (IntRange 1 1)] [NAry AllDifferent ["a", "b"]]
  assert "allDifferent rejects duplicate values" $ solveOne p == Right Nothing

testMapColouringSolves :: IO Bool
testMapColouringSolves = case parseProgram australia of
  Right program -> case solveOne program of
    Right (Just sol) -> assert "Australia map colouring solves" (satisfiesAll constraintsFromAustralia sol)
    _ -> assert "Australia map colouring solves" False
  Left _ -> assert "Australia map colouring solves" False

testUnsat :: IO Bool
testUnsat = do
  let p = Program [VarDecl "x" (IntRange 1 1), VarDecl "y" (IntRange 1 1)] [Binary NEq "x" "y"]
  assert "unsatisfiable CSP reports no solution" (solveOne p == Right Nothing)

testTinyQueens :: IO Bool
testTinyQueens = do
  let src = unlines
        [ "var q1, q2, q3, q4 : 1..4;"
        , "constraint allDifferent [q1, q2, q3, q4];"
        , "constraint q1 /= q2;" -- redundant, parser exercise
        , "solve;"
        ]
  case parseProgram src of
    Right program -> case solveOne program of
      Right (Just sol) -> assert "tiny allDifferent problem solves" (satisfiesAll [NAry AllDifferent ["q1", "q2", "q3", "q4"]] sol)
      _ -> assert "tiny allDifferent problem solves" False
    Left _ -> assert "tiny allDifferent problem solves" False

constraintsFromAustralia :: [Constraint]
constraintsFromAustralia = case parseProgram australia of
  Right (Program _ cs) -> cs
  _ -> []

australia :: String
australia = unlines
  [ "// Map colouring (Australia)"
  , "var WA, NT, SA, Q, NSW, V, T : { red, green, blue };"
  , "constraint WA  /= NT;"
  , "constraint WA  /= SA;"
  , "constraint NT  /= SA;"
  , "constraint NT  /= Q;"
  , "constraint SA  /= Q;"
  , "constraint SA  /= NSW;"
  , "constraint SA  /= V;"
  , "constraint Q   /= NSW;"
  , "constraint NSW /= V;"
  , "solve;"
  ]
