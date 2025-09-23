module ReadErrDo.Redo where 

import EithErrDo.Edo (IOEither, Error(..))

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

run :: ReadErrDo cfg e a -> cfg -> IOEither e a
run = unReadErrDo      

load :: cfg -> ReadErrDo cfg e a -> IOEither e a 
load = flip run
    
ask :: ReadErrDo r e r 
ask = ReadErrDo $ \r -> pure (Right r)

local :: (r -> r) -> ReadErrDo r e a -> ReadErrDo r e a
local f (ReadErrDo m) = ReadErrDo $ \r -> m (f r)

err :: Error e => e -> ReadErrDo r e a
err = ReadErrDo . const . pure . Left

liftIO :: IO a -> ReadErrDo r e a
liftIO = ReadErrDo . const . fmap Right 

liftIOEither :: Error e => IOEither e a -> ReadErrDo r e a
liftIOEither = ReadErrDo . const

liftEither :: Error e => Either e a -> ReadErrDo r e a
liftEither = ReadErrDo . const . pure

view :: Error e => (r -> a) -> ReadErrDo r e a
view f = ReadErrDo $ \r -> pure (Right (f r))

mapError :: Error e' => (e -> e') -> ReadErrDo r e a -> ReadErrDo r e' a
mapError f (ReadErrDo m) =
  ReadErrDo $ \r -> do
    ea <- m r
    pure $ case ea of
      Left  e -> Left (f e)
      Right a -> Right a

recover :: Error e => (e -> ReadErrDo r e a) -> ReadErrDo r e a -> ReadErrDo r e a
recover h (ReadErrDo m) =
  ReadErrDo $ \r -> do
    ea <- m r
    case ea of
      Left  e -> unReadErrDo (h e) r
      Right a -> pure (Right a)

hoistConfig :: Error e => (r' -> r) -> ReadErrDo r e a -> ReadErrDo r' e a
hoistConfig f (ReadErrDo m) =
  ReadErrDo $ \r' -> m (f r')
