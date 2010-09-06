module Data.BPlusTree where

import Data.Map ( Map )

-- | The unconventional structure of the B+ Tree means
-- there are internal nodes inside the tree that are different
-- than the leaf nodes which only appear at the deepest evel of
-- the tree.  We use a type structure that enforces the invariant.
data BPlus k a = L (Leaf k a)
               | N (BPlus k (Node k a))
data Leaf k a = Leaf { elements :: Map k a -- | TODO: change this to Data.Vector?
                     , next     :: Maybe (Leaf k a)
                     }
data Node k a = Node { parent  :: Node k a
                     , lesser  :: [Node k a] -- | length between n and 2n (inclusive)
                     , greater :: [Node k a] -- | length between n and 2n (inclusive)
                     }
