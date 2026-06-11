{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  System.Random.Stateful.Monad
-- Copyright   :  (c) Alexey Kuleshevich 2026
-- License     :  BSD-style (see the file LICENSE in the 'random' repository)
-- Maintainer  :  libraries@haskell.org
module System.Random.Stateful.Monad (
  MonadStatefulGen (..),
  HasGenEnv (..),
  R.StatefulGen,
  localSplitGen,

  -- * Uniform generation
  uniformM,
  R.Uniform,
  uniformRM,
  R.UniformRange,

  -- ** Lists
  uniformListM,
  uniformListRM,
  uniformShuffleListM,

  -- ** Generators for sequences of pseudo-random bytes
  uniformByteArrayM,
  uniformByteStringM,
  uniformShortByteStringM,
) where

import Control.Monad ((>=>))
import Control.Monad.Cont (ContT (..))
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import Control.Monad.Reader (MonadReader (ask, local), ReaderT (..))
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.Writer.Lazy as Lazy
import qualified Control.Monad.Writer.Strict as Strict
import Data.Array.Byte (ByteArray (..))
import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import qualified System.Random.Stateful as R
#if MIN_VERSION_mtl(2, 3, 0)
import Control.Monad.Accum (AccumT (..))
import qualified Control.Monad.RWS.CPS as CPS
import qualified Control.Monad.Writer.CPS as CPS
#endif

-- | `MonadReader` like type class designed specifically for `R.StatefulGen`. The reason for
-- creating a separate class, instead of relying on `MonadReader` is to allow transformers like
-- @RandT@ to be able to use this interface.
class (R.StatefulGen g m, HasGenEnv env g) => MonadStatefulGen env g m | m -> env where
  askGen :: m g
  default askGen :: MonadReader env m => m g
  askGen = getGenEnv <$> ask
  {-# INLINE askGen #-}

  -- | Overwite mutable generator in the environment
  localGenM :: (MonadReader env m, HasGenEnv env g) => (g -> m g) -> m a -> m a
  localGenM changeGen action = do
    g <- changeGen =<< askGen
    local (setGenEnv (const g)) action
  {-# INLINE localGenM #-}

class HasGenEnv env g | env -> g where
  -- | Extract generator from the environment
  getGenEnv :: env -> g

  -- | Overwrite generator in the environment. Used for `localGen` and is only useful with true
  -- mutable generators like `R.IOGenM`, etc.
  setGenEnv :: (g -> g) -> env -> env

instance HasGenEnv (R.StateGenM g) (R.StateGenM g) where
  getGenEnv = id
  setGenEnv = id

instance HasGenEnv (R.AtomicGenM g) (R.AtomicGenM g) where
  getGenEnv = id
  setGenEnv = id

instance HasGenEnv (R.IOGenM g) (R.IOGenM g) where
  getGenEnv = id
  setGenEnv = id

instance HasGenEnv (R.STGenM g s) (R.STGenM g s) where
  getGenEnv = id
  setGenEnv = id

instance HasGenEnv (R.TGenM g) (R.TGenM g) where
  getGenEnv = id
  setGenEnv = id

instance
  (Monad m, HasGenEnv env g, R.StatefulGen g (ReaderT env m)) =>
  MonadStatefulGen env g (ReaderT env m)

instance
  (MonadReader env m, HasGenEnv env g, R.StatefulGen g (ContT env m)) =>
  MonadStatefulGen env g (ContT env m)

instance
  (MonadReader env m, HasGenEnv env g, R.StatefulGen g (Lazy.StateT s m)) =>
  MonadStatefulGen env g (Lazy.StateT s m)

instance
  (MonadReader env m, HasGenEnv env g, R.StatefulGen g (Strict.StateT s m)) =>
  MonadStatefulGen env g (Strict.StateT s m)

instance
  (MonadReader env m, HasGenEnv env g, R.StatefulGen g (Lazy.WriterT w m), Monoid w) =>
  MonadStatefulGen env g (Lazy.WriterT w m)

instance
  (MonadReader env m, HasGenEnv env g, R.StatefulGen g (Strict.WriterT w m), Monoid w) =>
  MonadStatefulGen env g (Strict.WriterT w m)

instance
  (Monad m, HasGenEnv env g, R.StatefulGen g (Lazy.RWST env w s m), Monoid w) =>
  MonadStatefulGen env g (Lazy.RWST env w s m)

instance
  (Monad m, HasGenEnv env g, R.StatefulGen g (Strict.RWST env w s m), Monoid w) =>
  MonadStatefulGen env g (Strict.RWST env w s m)

#if MIN_VERSION_mtl(2, 3, 0)
instance
  (MonadReader env m, HasGenEnv env g, Monoid w, R.StatefulGen g (AccumT w m)) =>
  MonadStatefulGen env g (AccumT w m)

instance
  (R.StatefulGen g (CPS.WriterT w m), Monoid w, HasGenEnv env g, MonadReader env m) =>
  MonadStatefulGen env g (CPS.WriterT w m)

instance
  (R.StatefulGen g (CPS.RWST env w s m), Monoid w, HasGenEnv env g) =>
  MonadStatefulGen env g (CPS.RWST env w s m)
#endif

localSplitGen ::
  ( R.SplitGen gen
  , R.ThawedGen gen m
  , MonadStatefulGen env (R.MutableGen gen m) m
  , MonadReader env m
  ) =>
  m a -> m a
localSplitGen = localGenM (R.splitGenM >=> R.thawGen)

-- | Same as `R.uniformM`, but for `MonadStatefulGen`
--
-- @since 1.3.2
uniformM ::
  forall a g env m.
  ( MonadStatefulGen env g m
  , R.Uniform a
  ) =>
  m a
uniformM = askGen >>= R.uniformM
{-# INLINE uniformM #-}

-- | Same as `R.uniformRM`, but for `MonadStatefulGen`
--
-- @since 1.3.2
uniformRM ::
  forall a g env m.
  ( MonadStatefulGen env g m
  , R.UniformRange a
  ) =>
  (a, a) ->
  m a
uniformRM r = askGen >>= R.uniformRM r
{-# INLINE uniformRM #-}

-- | Same as `R.uniformListM`, but for `MonadStatefulGen`
--
-- @since 1.3.2
uniformListM ::
  forall a g env m.
  ( MonadStatefulGen env g m
  , R.Uniform a
  ) =>
  Int ->
  m [a]
uniformListM n = askGen >>= R.uniformListM n
{-# INLINE uniformListM #-}

-- | Same as `R.uniformListRM`, but for `MonadStatefulGen`
--
-- @since 1.3.2
uniformListRM ::
  forall a g env m.
  (MonadStatefulGen env g m, R.UniformRange a) =>
  Int ->
  (a, a) ->
  m [a]
uniformListRM r n = askGen >>= R.uniformListRM r n
{-# INLINE uniformListRM #-}

-- | Same as `R.uniformShuffleListM`, but for `MonadStatefulGen`
--
-- @since 1.3.2
uniformShuffleListM ::
  forall a g env m.
  MonadStatefulGen env g m =>
  [a] ->
  m [a]
uniformShuffleListM n = askGen >>= R.uniformShuffleListM n
{-# INLINE uniformShuffleListM #-}

-- | Same as `R.uniformByteArrayM`, but for `MonadStatefulGen`
--
-- @since 1.3.2
uniformByteArrayM ::
  forall g env m.
  MonadStatefulGen env g m =>
  Bool ->
  Int ->
  m ByteArray
uniformByteArrayM isPinned n = askGen >>= R.uniformByteArrayM isPinned n
{-# INLINE uniformByteArrayM #-}

-- | Same as `R.uniformByteStringM`, but for `MonadStatefulGen`
--
-- @since 1.3.2
uniformByteStringM ::
  forall g env m.
  MonadStatefulGen env g m =>
  Int -> m ByteString
uniformByteStringM n = askGen >>= R.uniformByteStringM n
{-# INLINE uniformByteStringM #-}

-- | Same as `R.uniformShortByteStringM`, but for `MonadStatefulGen`
--
-- @since 1.3.2
uniformShortByteStringM ::
  forall g env m.
  MonadStatefulGen env g m =>
  Int -> m ShortByteString
uniformShortByteStringM n = askGen >>= R.uniformShortByteStringM n
{-# INLINE uniformShortByteStringM #-}
