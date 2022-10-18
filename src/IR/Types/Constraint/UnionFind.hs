{-# OPTIONS_GHC -funbox-strict-fields #-}

{- | Generic implementaion of union find.

This implementation comes from: https://github.com/nominolo/union-find
-}

module IR.Types.Constraint.UnionFind
  ( fresh
  , repr
  , union
  , union'
  , equivalent
  , redundant
  , descriptor
  , setDescriptor
  , modifyDescriptor
  ) where

import           Control.Monad                  ( when )
import           Control.Monad.ST.Trans         ( STRef
                                                , newSTRef
                                                , readSTRef
                                                , writeSTRef
                                                )
import           IR.Types.Constraint.Type       ( Infer
                                                , Info(..)
                                                , Link(..)
                                                , Point(..)
                                                )


-- | /O(1)/. Create a fresh point and return it.  A fresh point is in
-- the equivalence class that contains only itself.
fresh :: a -> Infer s (Point s a)
fresh desc = do
  info <- newSTRef (MkInfo { weight = 1, descr = desc })
  l    <- newSTRef (Info info)
  return (Pt l)

-- | /O(1)/. @repr point@ returns the representative point of
-- @point@'s equivalence class.
--
-- This method performs the path compresssion.
repr :: Point s a -> Infer s (Point s a)
repr point@(Pt l) = do
  link <- readSTRef l
  case link of
    Info _           -> return point
    Link pt'@(Pt l') -> do
      pt'' <- repr pt'
      when (pt'' /= pt') $ do
        -- At this point we know that @pt'@ is not the representative
        -- element of @point@'s equivalent class.  Therefore @pt'@'s
        -- link must be of the form @Link r@.  We write this same
        -- value into @point@'s link reference and thereby perform
        -- path compression.
        link' <- readSTRef l'
        writeSTRef l link'
      return pt''

-- | Return the reference to the point's equivalence class's
-- descriptor.
descrRef :: Point s a -> Infer s (STRef s (Info a))
descrRef point@(Pt link_ref) = do
  link <- readSTRef link_ref
  case link of
    Info info           -> return info
    Link (Pt link'_ref) -> do
      link' <- readSTRef link'_ref
      case link' of
        Info info -> return info
        _         -> descrRef =<< repr point

-- | /O(1)/. Return the descriptor associated with argument point's
-- equivalence class.
descriptor :: Point s a -> Infer s a
descriptor point = do
  descr <$> (readSTRef =<< descrRef point)

-- | /O(1)/. Replace the descriptor of the point's equivalence class
-- with the second argument.
setDescriptor :: Point s a -> a -> Infer s ()
setDescriptor point new_descr = do
  r <- descrRef point
  modifySTRef r $ \i -> i { descr = new_descr }

modifyDescriptor :: Point s a -> (a -> a) -> Infer s ()
modifyDescriptor point f = do
  r <- descrRef point
  modifySTRef r $ \i -> i { descr = f (descr i) }

-- | /O(1)/. Join the equivalence classes of the points (which must be
-- distinct).  The resulting equivalence class will get the descriptor
-- of the second argument.
union :: Point s a -> Point s a -> Infer s ()
union p1 p2 = union' p1 p2 (\_ d2 -> return d2)

-- | Like 'union', but sets the descriptor returned from the callback.
--
-- The intention is to keep the descriptor of the second argument to
-- the callback, but the callback might adjust the information of the
-- descriptor or perform side effects.
union' :: Point s a -> Point s a -> (a -> a -> Infer s a) -> Infer s ()
union' p1 p2 update = do
  point1@(Pt link_ref1) <- repr p1
  point2@(Pt link_ref2) <- repr p2
  -- The precondition ensures that we don't create cyclic structures.
  when (point1 /= point2) $ do
    Info info_ref1 <- readSTRef link_ref1
    Info info_ref2 <- readSTRef link_ref2
    MkInfo w1 d1   <- readSTRef info_ref1 -- d1 is discarded
    MkInfo w2 d2   <- readSTRef info_ref2
    d2'            <- update d1 d2
    -- Make the smaller tree a a subtree of the bigger one.  The idea
    -- is this: We increase the path length of one set by one.
    -- Assuming all elements are accessed equally often, this means
    -- the penalty is smaller if we do it for the smaller set of the
    -- two.
    if w1 >= w2
      then do
        writeSTRef link_ref2 (Link point1)
        writeSTRef info_ref1 (MkInfo (w1 + w2) d2')
      else do
        writeSTRef link_ref1 (Link point2)
        writeSTRef info_ref2 (MkInfo (w1 + w2) d2')

-- | /O(1)/. Return @True@ if both points belong to the same
-- | equivalence class.
equivalent :: Point s a -> Point s a -> Infer s Bool
equivalent p1 p2 = (==) <$> repr p1 <*> repr p2

-- | /O(1)/. Returns @True@ for all but one element of an equivalence
-- class.  That is, if @ps = [p1, .., pn]@ are all in the same
-- equivalence class, then the following assertion holds.
--
-- > do rs <- mapM redundant ps
-- >    assert (length (filter (==False) rs) == 1)
--
-- It is unspecified for which element function returns @False@, so be
-- really careful when using this.
redundant :: Point s a -> Infer s Bool
redundant (Pt link_r) = do
  link <- readSTRef link_r
  case link of
    Info _ -> return False
    Link _ -> return True

modifySTRef :: STRef s a -> (a -> a) -> Infer s ()
modifySTRef ref f = do
  x <- readSTRef ref
  writeSTRef ref (f x)
