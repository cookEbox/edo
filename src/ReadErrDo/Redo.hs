{-|
Module      : ReadErrDo.Redo
Description : Core definition and combinators for the ReadErrDo environment
License     : BSD-3-Clause
Maintainer  : nj.cooke@outlook.com
Stability   : experimental

This is the minimal Reader-like layer for @IO (Either e a)@ programs used by
'ReadErrDo.Edo'. Most users should import @ReadErrDo.Edo@ instead.

@since 0.1.0
-}

module ReadErrDo.Redo where 

import EithErrDo.Edo (IOEither, Error(..))

-- $setup
-- >>> import Prelude hiding ((>>=), (>>), return)
-- >>> import qualified EithErrDo.Edo as E
-- >>> :set -XQualifiedDo
-- >>> :set -XDerivingVia
-- >>> :{ 
-- newtype TestError = TestError String deriving (Eq, Show)
-- deriving via (E.ViaShow TestError) instance E.Error TestError 
-- :}

newtype ReadErrDo cfg e a 
  = ReadErrDo { unReadErrDo :: cfg -> IOEither e a 
              } 

instance Functor (ReadErrDo cfg e) where 
  fmap f (ReadErrDo g) = ReadErrDo $ fmap (fmap f) . g
  
instance Applicative (ReadErrDo cfg e) where 
  pure                          = ReadErrDo . const . pure . Right
  ReadErrDo mf <*> ReadErrDo mg = ReadErrDo $ \c -> do
    ef <- mf c
    case ef of 
      Left e -> return $ Left e
      Right f -> fmap f <$> mg c
        
instance Monad (ReadErrDo cfg e) where 
  return = pure
  ReadErrDo ma >>= emf = ReadErrDo $ \c -> do 
    ea <- ma c
    case ea of 
      Left e -> return $ Left e
      Right a -> unReadErrDo (emf a) c 

-- >>> :{
-- let m :: ReadErrDo Int TestError Int
--     m = do
--       x <- R.view (+1)
--       R.liftIOEither (E.ok (x * 2 :: Int))
-- in R.run m 3
-- :}
-- Right 8
run :: ReadErrDo cfg e a -> cfg -> IOEither e a
run = unReadErrDo      

-- >>> :{
-- let m :: ReadErrDo String TestError String
--     m = view (<> " world")
-- in load "hello" m
-- :}
-- Right "hello world"
load :: cfg -> ReadErrDo cfg e a -> IOEither e a 
load = flip run
    
-- | Access the whole config.
--
-- >>> run (ask :: ReadErrDo String TestError String) "cfg"
-- Right "cfg"
ask :: ReadErrDo r e r 
ask = ReadErrDo $ \r -> pure (Right r)

-- | Locally tweak the config.
--
-- >>> run (local (+1) (ask :: ReadErrDo Int TestError Int)) 41
-- Right 42
local :: (r -> r) -> ReadErrDo r e a -> ReadErrDo r e a
local f (ReadErrDo m) = ReadErrDo $ \r -> m (f r)

-- | Construct a failure inside 'ReadErrDo'.
err :: Error e => e -> ReadErrDo r e a
err = ReadErrDo . const . pure . Left

-- | Lift plain IO.
liftIO :: IO a -> ReadErrDo r e a
liftIO = ReadErrDo . const . fmap Right 

-- | Lift 'IOEither'.
liftIOEither :: Error e => IOEither e a -> ReadErrDo r e a
liftIOEither = ReadErrDo . const

-- | Lift pure 'Either'.
--
-- >>> run (liftEither (Right (7 :: Int)) :: ReadErrDo () TestError Int) ()
-- Right 7
-- >>> run (liftEither (Left (TestError "oops") :: Either TestError Int)) ()
-- Left (TestError "oops")
liftEither :: Error e => Either e a -> ReadErrDo r e a
liftEither = ReadErrDo . const . pure

-- | Project from config.
--
-- >>> run (view length :: ReadErrDo String TestError Int) "abcd"
-- Right 4
view :: Error e => (r -> a) -> ReadErrDo r e a
view f = ReadErrDo $ \r -> pure (Right (f r))

-- >>> :{
-- let m = liftEither (Left (TestError "bad")) :: ReadErrDo () TestError ()
-- in  run (mapError (fmap length) m) ()
-- :}
-- Left 3
mapError :: Error e' => (e -> e') -> ReadErrDo r e a -> ReadErrDo r e' a
mapError f (ReadErrDo m) =
  ReadErrDo $ \r -> do
    ea <- m r
    pure $ case ea of
      Left  e -> Left (f e)
      Right a -> Right a

-- | Recover from an error.
--
-- >>> :{
-- let boom = liftEither (Left (TestError "x"):: Either TestError Int)
-- in  run (recover (\_ -> pure 42) boom) ()
-- :}
-- Right 42
recover :: Error e => (e -> ReadErrDo r e a) -> ReadErrDo r e a -> ReadErrDo r e a
recover h (ReadErrDo m) =
  ReadErrDo $ \r -> do
    ea <- m r
    case ea of
      Left  e -> unReadErrDo (h e) r
      Right a -> pure (Right a)

-- | Change the config type contravariantly.
--
-- >>> :{
-- let m = view (\(a,b) -> a + b) :: ReadErrDo (Int,Int) TestError Int
-- in  run (hoistConfig (\x -> (x,x)) m) (10 :: Int)
-- :}
-- Right 20
hoistConfig :: Error e => (r' -> r) -> ReadErrDo r e a -> ReadErrDo r' e a
hoistConfig f (ReadErrDo m) =
  ReadErrDo $ \r' -> m (f r')
