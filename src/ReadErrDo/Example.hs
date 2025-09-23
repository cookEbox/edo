{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QualifiedDo #-}
module ReadErrDo.Example where

import           EithErrDo.Edo      (Error (..), IOEither, ViaShow, fromMaybeE,
                                     hoistEither, ok)
import qualified EithErrDo.Edo      as E
import           ReadErrDo.Redo

import           Control.Monad      (void)
import           Data.Text          (Text, pack)
import           System.Environment (lookupEnv)

t :: String -> Text
t = pack

ts :: [String] -> Text
ts = t . concat

s :: Show a => a -> String
s = show

data AppErr
  = MissingVar Text
  | BadPort Text
  | DbErr Text
  deriving (Eq, Show)
  deriving Error via (ViaShow AppErr)

data Config = Config
  { fallbackPort :: Int
  , defaultUser  :: Text
  }

parsePort :: String -> Either AppErr Int
parsePort st =
  case reads st of
    [(n,"")] | n > 0 && n < 65536 -> Right n
    _                             -> Left (BadPort (t st))

fetchGreeting :: Text -> IOEither AppErr Text
fetchGreeting who = E.do
  ok (t "Hello, " <> who <> t "!")  -- could be a real IO action in E.do

program :: ReadErrDo Config AppErr Text
program = do
  cfg <- ask
  let fallback = fallbackPort cfg
      who      = defaultUser  cfg

  mEnv <- liftIO (lookupEnv "PORT")
  port <- recover (const $ pure fallback) $ do
    portStr <- liftIOEither (fromMaybeE (MissingVar (t "PORT")) mEnv)
    liftEither (parsePort portStr)

  void . liftIOEither $ hoistEither BadPort (Right (show port))
  liftIOEither (fetchGreeting who)

main :: IO ()
main = do
  let cfg = Config { fallbackPort = 8080, defaultUser = t "Nick" }
  r <- load cfg program
  case r of 
    Left e -> putStrLn $ "ERROR: " <> show e
    Right m -> print m

