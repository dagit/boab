{-# LANGUAGE GADTs, EmptyDataDecls, ScopedTypeVariables #-}
module Data.BPlusTree where

-- import qualified System.IO as S ( Handle, IOMode(..), hSeek,
--                                  hTell, SeekMode(..) )
import qualified System.IO as S ( Handle, IOMode(..) )
import Foreign ( ForeignPtr )
import Data.Map ( Map )
import qualified Data.Map as M ( empty, toList, lookup, elemAt
                               , size, elemAt, split, insert
                               , fromAscList )
import Prelude hiding ( lookup )
-- import Control.Applicative ( (<$>) )


-- | B+ Tree
data BPlus k a = BPLeaf (Map k a) (Maybe (BPlus k a))
               | BPNode (Map k (BPlus k a))

data BPlusTree k a = BPlusTree { order :: Int
                               , tree  :: BPlus k a
                               }

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

-- | Creates an empty B+ tree with order @n@.
-- The nodes of the tree will have between @n@ and @2*n@ elements (inclusive).
empty :: Int -> BPlusTree k a
empty o = BPlusTree o (BPLeaf M.empty Nothing)

toList :: BPlusTree k a -> [(k, a)]
toList (BPlusTree _ t) = go t
  where
  go :: BPlus k a -> [(k, a)]
  go (BPLeaf m Nothing)  = M.toList m
  go (BPLeaf m (Just l)) = M.toList m ++ go l
  go (BPNode cs)
    | M.size cs == 0 = [] -- Odd case, should be impossible
    | otherwise      = go (snd (M.elemAt 0 cs))

lookup :: forall k a. Ord k => k -> BPlusTree k a -> Maybe a
lookup k (BPlusTree _ t) = go t
  where
  go :: BPlus k a -> Maybe a
  go (BPLeaf m _) = M.lookup k m
  go (BPNode cs)
    | isEmpty cs      = Nothing -- Odd case, should be impossible
    | isEmpty l       = go (snd (M.elemAt 0 cs))
    | isEmpty r       = Nothing -- k too large
    | otherwise       = go (snd (M.elemAt 0 r))
    where
    (l, r)    = M.split k cs
    isEmpty m = M.size m == 0

insert :: forall k a. Ord k => k -> a -> BPlusTree k a -> BPlusTree k a
insert k a (BPlusTree o t) = BPlusTree o (snd . go $ t)
  where
  -- Returns @k@ when the parent needs to update.
  -- This happens when the leaf was split.
  go :: BPlus k a -> (Maybe k, BPlus k a)
  go (BPLeaf m n)
     -- TODO: This still isn't correct
    | M.size m < 2*o = (Nothing, BPLeaf (M.insert k a m) n)
    | otherwise      = (Just (fst (M.elemAt 0 r)), node)
    where
    (l,r) = M.split (fst (M.elemAt o m)) m
    insert' :: Map k a -> Map k a -> (Map k a, Map k a)
    insert' x y = case compare k (fst (M.elemAt 0 y)) of
                  LT -> (M.insert k a x, y)
                  _  -> (x, M.insert k a y)
    node = let (p, q) = insert' l r
               lk = fst (M.elemAt 0 p)
               rk = fst (M.elemAt 0 q)
               lp = BPLeaf p (Just rq)
               rq = BPLeaf q Nothing
               in BPNode (M.fromAscList [(lk,lp),(rk,rq)])
  go (BPNode cs)
    | isEmpty cs = error "insert: malformed BPlus tree"
    | isEmpty l  = go (snd (M.elemAt 0 cs)) -- TODO: handle split
    | isEmpty r  = error "insert: malformed BPlus tree"
    | otherwise  = go (snd (M.elemAt 0 r))  -- TODO: handle split
    where
    (l, r)    = M.split k cs
    isEmpty m = M.size m == 0