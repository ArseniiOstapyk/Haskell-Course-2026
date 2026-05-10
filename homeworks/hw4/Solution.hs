newtype Reader r a = Reader { runReader :: r -> a }

-- Task 1
instance Functor (Reader r) where
  fmap f reader = Reader $ \env ->
    f (runReader reader env)

instance Applicative (Reader r) where
  pure x = Reader $ \_ -> x

  liftA2 f readerA readerB = Reader $ \env ->
    f (runReader readerA env) (runReader readerB env)

instance Monad (Reader r) where
  reader >>= f = Reader $ \env ->
    runReader (f (runReader reader env)) env

-- Task 2
ask :: Reader r r
ask = Reader $ \env -> env

asks :: (r -> a) -> Reader r a
asks f = Reader $ \env -> f env

local :: (r -> r) -> Reader r a -> Reader r a
local f reader = Reader $ \env ->
  runReader reader (f env)

-- Task 3
data BankConfig = BankConfig
  { interestRate :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  } deriving (Show)

data Account = Account
  { accountId :: String
  , balance :: Int
  } deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  pure (floor (fromIntegral (balance acc) * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  pure acc { balance = balance acc - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minBal <- asks minimumBalance
  pure (balance acc >= minBal)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  updatedAccount <- applyTransactionFee acc
  interest <- calculateInterest acc
  meetsMinimum <- checkMinimumBalance acc
  pure (updatedAccount, interest, meetsMinimum)