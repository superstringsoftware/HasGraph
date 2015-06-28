import Prelude hiding (lookup)

import Control.Monad.ST
import Data.HashTable.ST.Basic

-- Hashtable parameterized by ST "thread"
type HT s = HashTable s String String

set :: ST s (HT s)
set = do
  ht <- new
  insert ht "key" "value1"
  insert ht "key1" "value2"
  return ht

-- set1 :: (HashTable s String String)
-- set1 = runST $ set

get :: String -> HT s -> ST s (Maybe String)
get s ht = do
  val <- lookup ht s
  return val

example :: Maybe String
example = runST (set >>= get "key1")