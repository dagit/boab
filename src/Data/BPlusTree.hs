{-# LANGUAGE GADTs, EmptyDataDecls #-}
module Data.BPlusTree where

-- import qualified System.IO as S ( Handle, IOMode(..), hSeek,
--                                  hTell, SeekMode(..) )
import qualified System.IO as S ( Handle, IOMode(..) )
import Foreign ( ForeignPtr )
import Data.Map ( Map )
import qualified Data.Map as M ( empty, toList, lookup, elemAt
                               , size, elemAt, split )
import Prelude hiding ( lookup )
-- import Control.Applicative ( (<$>) )


-- | B+ Tree
data Leaf
data Node

data BPlus t k a where
  BPLeaf :: Map k a             -> Maybe (BPlus Leaf k a) -> BPlus Leaf k a
  BPNode :: Map k (BPlus t k a) -> BPlus Node k a

-- | Infrastructure for managing B+ Tree on disk
-- Not yet used or written.
newtype Offset a = BP Integer

data Location = MemoryLocation
              | FileLocation FilePath S.IOMode

data Handle a = FileHandle S.Handle
              | MemoryHandle Integer (ForeignPtr a)
{-

seek :: Handle a -> Offset a -> IO ()
seek (FileHandle h)   (BP i) = S.hSeek h S.AbsoluteSeek i
seek (MemoryHandle h) (BP i) = 
tell :: Handle a -> IO (Offset a)
tell (FileHandle h) = BP <$> S.hTell h

-}

-- | Pure API
empty :: BPlus Leaf k a
empty = BPLeaf M.empty Nothing

toList :: BPlus Leaf k a -> [(k, a)]
toList (BPLeaf m Nothing)  = M.toList m
toList (BPLeaf m (Just l)) = M.toList m ++ toList l

lookup :: Ord k => k -> BPlus t k a -> Maybe a
lookup k (BPLeaf m _) = M.lookup k m
lookup k (BPNode cs)
  | isEmpty cs      = Nothing -- Odd case
  | isEmpty l       = lookup k (snd (M.elemAt 0 cs))
  | isEmpty r       = Nothing -- k too large
  | otherwise       = lookup k (snd (M.elemAt 0 r))
  where
  (l, r)    = M.split k cs
  isEmpty m = M.size m == 0
