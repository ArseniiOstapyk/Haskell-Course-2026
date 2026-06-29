module Main where

import CSP.AST (prettyAssignment)
import CSP.Parser (parseProgram, renderParseError)
import CSP.Solver (solveAll, solveOne)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> runFile False file
    ["--all", file] -> runFile True file
    _ -> die "Usage: csp [--all] FILE.csp"

runFile :: Bool -> FilePath -> IO ()
runFile allSolutions file = do
  src <- readFile file
  program <- case parseProgram src of
    Left err -> die ("Parse error: " ++ renderParseError err)
    Right p -> pure p
  if allSolutions
    then case solveAll program of
      Left err -> die ("Solver error: " ++ err)
      Right [] -> putStrLn "unsatisfiable"
      Right sols -> do
        putStrLn ("solutions: " ++ show (length sols))
        mapM_ (putStrLn . prettyAssignment) sols
    else case solveOne program of
      Left err -> die ("Solver error: " ++ err)
      Right Nothing -> putStrLn "unsatisfiable"
      Right (Just solution) -> putStrLn (prettyAssignment solution)
