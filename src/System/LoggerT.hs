-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.LoggerT
    ( LoggerT
    , MonadLogger (..)
    , runLoggerT

    , L.Level    (..)
    , L.Output   (..)
    , L.Settings (..)
    , L.Logger
    , L.DateFormat

    , L.new
    , L.create
    , L.defSettings
    , L.iso8601UTC

    , module M
    )
where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad.Catch
import Control.Monad.Reader
import System.Logger (Logger, Level (..))
import System.Logger.Message as M

import qualified System.Logger as L

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

    prefix :: m (Msg -> Msg)
    prefix = return id

    log :: Level -> (Msg -> Msg) -> m ()
    log l m = do
        g <- logger
        p <- prefix
        L.log g l (p . m)

    logM :: Level -> m (Msg -> Msg) -> m ()
    logM l m = do
        g <- logger
        p <- prefix
        L.logM g l ((p .) `liftM` m)

    trace, debug, info, warn, err, fatal :: (Msg -> Msg) -> m ()
    trace = log Trace
    debug = log Debug
    info  = log Info
    warn  = log Warn
    err   = log Error
    fatal = log Fatal

    traceM, debugM, infoM, warnM, errM, fatalM :: m (Msg -> Msg) -> m ()
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
