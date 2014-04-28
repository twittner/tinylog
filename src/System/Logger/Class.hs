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

import System.Logger (Level (..))
import System.Logger.Message as M

import qualified System.Logger as L

class Monad m => MonadLogger m where
    log :: Level -> (Msg -> Msg) -> m ()

-- | Abbreviation for 'log' using the corresponding log level.
trace, debug, info, warn, err, fatal :: MonadLogger m => (Msg -> Msg) -> m ()
trace = System.Logger.Class.log Trace
debug = System.Logger.Class.log Debug
info  = System.Logger.Class.log Info
warn  = System.Logger.Class.log Warn
err   = System.Logger.Class.log Error
fatal = System.Logger.Class.log Fatal

