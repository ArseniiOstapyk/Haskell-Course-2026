import Control.Monad.State
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Data.Map (Map)

-- EX 1
data Instr = PUSH Int | POP | DUP | SWAP | ADD | MUL | NEG
  deriving (Show, Eq)

execInstr :: Instr -> State [Int] ()
execInstr (PUSH n) = modify (n :)
execInstr POP = do
  st <- get
  case st of
    (_:xs) -> put xs
    [] -> pure ()
execInstr DUP = do
  st <- get
  case st of
    (x:xs) -> put (x:x:xs)
    [] -> pure ()
execInstr SWAP = do
  st <- get
  case st of
    (x:y:xs) -> put (y:x:xs)
    _ -> pure ()
execInstr ADD = do
  st <- get
  case st of
    (x:y:xs) -> put ((y + x):xs)
    _ -> pure ()
execInstr MUL = do
  st <- get
  case st of
    (x:y:xs) -> put ((y * x):xs)
    _ -> pure ()
execInstr NEG = do
  st <- get
  case st of
    (x:xs) -> put ((-x):xs)
    [] -> pure ()

execProg :: [Instr] -> State [Int] ()
execProg = mapM_ execInstr

runProg :: [Instr] -> [Int]
runProg p = execState (execProg p) []

-- EX 2
data Expr
  = Num Int
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Neg Expr
  | Assign String Expr
  | Seq Expr Expr
  deriving (Show, Eq)

eval :: Expr -> State (Map String Int) Int
eval (Num n) = pure n
eval (Var name) = do
  env <- get
  pure (env Map.! name)
eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  pure (v1 + v2)
eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  pure (v1 * v2)
eval (Neg e) = do
  v <- eval e
  pure (-v)
eval (Assign name e) = do
  v <- eval e
  modify (Map.insert name v)
  pure v
eval (Seq e1 e2) = do
  _ <- eval e1
  eval e2

runEval :: Expr -> Int
runEval e = evalState (eval e) Map.empty

-- EX 3
editDistM :: String -> String -> Int -> Int -> State (Map (Int, Int) Int) Int
editDistM xs ys i j = do
  cache <- get
  case Map.lookup (i, j) cache of
    Just value -> pure value
    Nothing -> do
      value <-
        if i == 0 then
          pure j
        else if j == 0 then
          pure i
        else if xs !! (i - 1) == ys !! (j - 1) then
          editDistM xs ys (i - 1) (j - 1)
        else do
          deletion <- editDistM xs ys (i - 1) j
          insertion <- editDistM xs ys i (j - 1)
          substitution <- editDistM xs ys (i - 1) (j - 1)
          pure (1 + minimum [deletion, insertion, substitution])
      modify (Map.insert (i, j) value)
      pure value

editDistance :: String -> String -> Int
editDistance xs ys =
  evalState (editDistM xs ys (length xs) (length ys)) Map.empty

-- EX 4
data Location
  = Normal
  | Decision [String]
  | Obstacle Int
  | Treasure Int
  | Trap Int
  | Goal
  deriving (Show, Eq)

data GameState = GameState
  { playerPosition :: String
  , playerEnergy :: Int
  , playerScore :: Int
  , boardMap :: Map String Location
  , pathMap :: Map String (Map String String)
  } deriving (Show, Eq)

type AdventureGame a = StateT GameState IO a

initialGameState :: GameState
initialGameState =
  GameState
    { playerPosition = "Start"
    , playerEnergy = 20
    , playerScore = 0
    , boardMap = Map.fromList
        [ ("Start", Decision ["Forest", "Cave"])
        , ("Forest", Treasure 10)
        , ("River", Obstacle 2)
        , ("Bridge", Trap 5)
        , ("Hill", Treasure 15)
        , ("Cave", Trap 8)
        , ("Tunnel", Obstacle 3)
        , ("Ruins", Treasure 20)
        , ("Gate", Normal)
        , ("Treasure", Goal)
        ]
    , pathMap = Map.fromList
        [ ("Start", Map.fromList [("Forest", "Forest"), ("Cave", "Cave")])
        , ("Forest", Map.fromList [("forward", "River")])
        , ("River", Map.fromList [("forward", "Bridge")])
        , ("Bridge", Map.fromList [("forward", "Hill")])
        , ("Hill", Map.fromList [("forward", "Treasure")])
        , ("Cave", Map.fromList [("forward", "Tunnel")])
        , ("Tunnel", Map.fromList [("forward", "Ruins")])
        , ("Ruins", Map.fromList [("forward", "Gate")])
        , ("Gate", Map.fromList [("forward", "Treasure")])
        , ("Treasure", Map.empty)
        ]
    }

movePlayer :: Int -> AdventureGame Int
movePlayer roll = do
  st <- get
  let energyLeft = max 0 (playerEnergy st - 1)
  put st { playerEnergy = energyLeft }
  moveSteps roll
  pure roll

moveSteps :: Int -> AdventureGame ()
moveSteps n
  | n <= 0 = pure ()
  | otherwise = do
      st <- get
      let pos = playerPosition st
      let paths = Map.findWithDefault Map.empty pos (pathMap st)
      if Map.null paths then
        pure ()
      else if Map.size paths == 1 then do
        let next = snd (head (Map.toList paths))
        put st { playerPosition = next }
        moveSteps (n - 1)
      else
        pure ()

makeDecision :: [String] -> AdventureGame String
makeDecision options = do
  choice <- liftIO (getPlayerChoice options)
  st <- get
  let pos = playerPosition st
  let paths = Map.findWithDefault Map.empty pos (pathMap st)
  case Map.lookup choice paths of
    Just next -> put st { playerPosition = next }
    Nothing -> pure ()
  pure choice

handleLocation :: AdventureGame Bool
handleLocation = do
  st <- get
  let pos = playerPosition st
  let loc = Map.findWithDefault Normal pos (boardMap st)
  case loc of
    Normal -> do
      liftIO (putStrLn "Nothing special happens here.")
      pure False
    Decision options -> do
      liftIO (putStrLn "You reached a decision point.")
      _ <- makeDecision options
      pure False
    Obstacle penalty -> do
      liftIO (putStrLn ("Obstacle! You lose " ++ show penalty ++ " energy."))
      modify (\s -> s { playerEnergy = max 0 (playerEnergy s - penalty) })
      pure False
    Treasure points -> do
      liftIO (putStrLn ("Treasure! You gain " ++ show points ++ " points."))
      modify (\s -> s { playerScore = playerScore s + points })
      pure False
    Trap points -> do
      liftIO (putStrLn ("Trap! You lose " ++ show points ++ " points."))
      modify (\s -> s { playerScore = max 0 (playerScore s - points) })
      pure False
    Goal -> do
      liftIO (putStrLn "You found the main treasure!")
      pure True

playTurn :: AdventureGame Bool
playTurn = do
  st <- get
  liftIO (displayGameState st)
  if playerEnergy st <= 0 then do
    liftIO (putStrLn "You are out of energy.")
    pure True
  else do
    roll <- liftIO getDiceRoll
    _ <- movePlayer roll
    ended <- handleLocation
    stAfter <- get
    liftIO (displayGameState stAfter)
    if playerEnergy stAfter <= 0 then do
      liftIO (putStrLn "You are out of energy.")
      pure True
    else
      pure ended

playGame :: AdventureGame ()
playGame = do
  ended <- playTurn
  if ended then do
    st <- get
    liftIO (putStrLn ("Final score: " ++ show (playerScore st)))
  else
    playGame

getDiceRoll :: IO Int
getDiceRoll = do
  putStrLn "Enter dice roll:"
  input <- getLine
  case reads input of
    [(n, "")] | n > 0 -> pure n
    _ -> do
      putStrLn "Invalid dice roll. Enter a positive integer."
      getDiceRoll

displayGameState :: GameState -> IO ()
displayGameState st = do
  putStrLn "-----------------------------"
  putStrLn ("Position: " ++ playerPosition st)
  putStrLn ("Energy:   " ++ show (playerEnergy st))
  putStrLn ("Score:    " ++ show (playerScore st))
  putStrLn "-----------------------------"

getPlayerChoice :: [String] -> IO String
getPlayerChoice options = do
  putStrLn "Choose one option:"
  mapM_ putStrLn options
  input <- getLine
  if input `elem` options then
    pure input
  else do
    putStrLn "Invalid choice."
    getPlayerChoice options

main :: IO ()
main = evalStateT playGame initialGameState