-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.LoggerT
    ( LoggerT
    , MonadLogger (..)
    , L.Logger
    , L.Level     (..)
    , runLoggerT
    )
where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Catch
import Data.ByteString (ByteString)
import Data.Monoid
import System.Logger (Logger, Level (..))

import qualified Data.ByteString as BS
import qualified System.Logger   as L

newtype LoggerT m a = LoggerT
    { unwrap :: ReaderT Logger m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadCatch
               , MonadReader Logger
               , MonadTrans
               )

class MonadIO m => MonadLogger m where
    logger :: m Logger

    prefix :: m ByteString
    prefix = return BS.empty

    log :: Level -> ByteString -> m ()
    log l m = do
        g <- logger
        p <- prefix
        L.log g l (p <> m)

    logM :: Level -> m ByteString -> m ()
    logM l m = do
        g <- logger
        p <- prefix
        L.logM g l ((<> p) `liftM` m)

    trace, debug, info, warn, err, fatal :: ByteString -> m ()
    trace = log Trace
    debug = log Debug
    info  = log Info
    warn  = log Warn
    err   = log Error
    fatal = log Fatal

    traceM, debugM, infoM, warnM, errM, fatalM :: m ByteString -> m ()
    traceM = logM Trace
    debugM = logM Debug
    infoM  = logM Info
    warnM  = logM Warn
    errM   = logM Error
    fatalM = logM Fatal

    flush :: m ()
    flush = logger >>= L.flush

    close :: m ()
    close = logger >>= L.close

instance MonadIO m => MonadLogger (LoggerT m) where
    logger = LoggerT ask

instance (MonadIO m, MonadReader Logger m) => MonadLogger (ReaderT r m) where
    logger = lift ask

runLoggerT :: MonadIO m => L.Logger -> LoggerT m a -> m a
runLoggerT l m = runReaderT (unwrap m) l
