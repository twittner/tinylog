-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE FlexibleContexts #-}

module System.Logger.Class
    ( MonadLogger (..)
    , trace
    , debug
    , info
    , warn
    , err
    , fatal

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
import Control.Monad.Reader
import System.Logger (Logger, Level (..))
import System.Logger.Message as M

import qualified System.Logger as L

class MonadIO m => MonadLogger m where
    log :: Level -> (Msg -> Msg) -> m ()

instance (MonadIO m, MonadReader Logger m) => MonadLogger (ReaderT r m) where
    log l m = lift ask >>= \g -> L.log g l m

-- | Abbreviation for 'log' using the corresponding log level.
trace, debug, info, warn, err, fatal :: MonadLogger m => (Msg -> Msg) -> m ()
trace = log Trace
debug = log Debug
info  = log Info
warn  = log Warn
err   = log Error
fatal = log Fatal

