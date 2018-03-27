
-- | Unique id creation (used for the API calls)

module Bitcoin.Misc.Unique where

--------------------------------------------------------------------------------

import Data.Word

import Control.Concurrent.MVar
import System.IO.Unsafe as Unsafe

--------------------------------------------------------------------------------

theGlobalUniqueSupport :: MVar Word64
theGlobalUniqueSupport = Unsafe.unsafePerformIO (newMVar 1000000000)

theGlobalSink :: MVar a
theGlobalSink = Unsafe.unsafePerformIO (newMVar undefined)

--------------------------------------------------------------------------------

newUnique :: IO Word64
newUnique = do
  n <- takeMVar theGlobalUniqueSupport
  putMVar theGlobalUniqueSupport (n+1)
  return n

-- | We convert a \"sacrifice\" to a unique id. This is a hack, but
-- sacrificing a value prevents let-floating, also it somehow makes sense :)
convertToUnique :: a -> Word64
convertToUnique sacrifice = Unsafe.unsafePerformIO $ do
  _ <- takeMVar theGlobalSink
  n <- newUnique
  putMVar theGlobalSink (sacrifice,n)
  return n

--------------------------------------------------------------------------------
