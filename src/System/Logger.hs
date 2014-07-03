-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

-- | Small layer on top of @fast-logger@ which adds log-levels and
-- timestamp support (using @date-cache@) and not much more.
module System.Logger
    ( Settings
    , defSettings
    , logLevel
    , setLogLevel
    , output
    , setOutput
    , format
    , setFormat
    , delimiter
    , setDelimiter
    , netstrings
    , setNetStrings
    , bufSize
    , setBufSize
    , name
    , setName

    , Level    (..)
    , Output   (..)

    , DateFormat
    , iso8601UTC

    , Logger
    , new
    , create
    , level
    , flush
    , close
    , clone
    , settings

    , log
    , trace
    , debug
    , info
    , warn
    , err
    , fatal

    , module M
    ) where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.UnixTime
import System.Date.Cache
import System.Environment (lookupEnv)
import System.Logger.Message as M
import System.Logger.Settings

import qualified System.Log.FastLogger as FL

data Logger = Logger
    { logger    :: FL.LoggerSet
    , settings  :: Settings
    , getDate   :: Maybe DateCacheGetter
    , closeDate :: Maybe DateCacheCloser
    }

-- | Create a new 'Logger' with the given 'Settings'.
-- Please note that the 'logLevel' can be dynamically adjusted by setting
-- the environment variable @LOG_LEVEL@ accordingly. Likewise the buffer
-- size can be dynamically set via @LOG_BUFFER@ and netstrings encoding
-- can be enabled with @LOG_NETSTR=True@
new :: MonadIO m => Settings -> m Logger
new s = liftIO $ do
    n <- fmap (readNote "Invalid LOG_BUFFER") <$> lookupEnv "LOG_BUFFER"
    l <- fmap (readNote "Invalid LOG_LEVEL")  <$> lookupEnv "LOG_LEVEL"
    e <- fmap (readNote "Invalid LOG_NETSTR") <$> lookupEnv "LOG_NETSTR"
    g <- fn (output s) (fromMaybe (bufSize s) n)
    c <- clockCache (format s)
    let s' = s { logLevel   = fromMaybe (logLevel s) l
               , netstrings = fromMaybe (netstrings s) e
               }
    return $ Logger g s' (fst <$> c) (snd <$> c)
  where
    fn StdOut   = FL.newStdoutLoggerSet
    fn StdErr   = FL.newStderrLoggerSet
    fn (Path p) = flip FL.newFileLoggerSet p

    clockCache "" = return Nothing
    clockCache f  = Just <$> clockDateCacher (DateCacheConf getUnixTime (fmt f))

    fmt :: DateFormat -> UnixTime -> IO ByteString
    fmt d = return . formatUnixTimeGMT (template d)

-- | Invokes 'new' with default settings and the given output as log sink.
create :: MonadIO m => Output -> m Logger
create p = new defSettings { output = p }

readNote :: Read a => String -> String -> a
readNote m s = case reads s of
    [(a, "")] -> a
    _         -> error m

-- | Logs a message with the given level if greater or equal to the
-- logger's threshold.
log :: MonadIO m => Logger -> Level -> (Msg -> Msg) -> m ()
log g l m = unless (level g > l) . liftIO $ putMsg g l m
{-# INLINE log #-}

-- | Abbreviation of 'log' using the corresponding log level.
trace, debug, info, warn, err, fatal :: MonadIO m => Logger -> (Msg -> Msg) -> m ()
trace g = log g Trace
debug g = log g Debug
info  g = log g Info
warn  g = log g Warn
err   g = log g Error
fatal g = log g Fatal
{-# INLINE trace #-}
{-# INLINE debug #-}
{-# INLINE info  #-}
{-# INLINE warn  #-}
{-# INLINE err   #-}
{-# INLINE fatal #-}

-- | Clone the given logger and optionally give it a name
-- (use @(Just \"\")@ to clear).
clone :: Maybe Text -> Logger -> Logger
clone (Just n) g = g { settings = setName n  (settings g) }
clone Nothing  g = g

-- | Force buffered bytes to output sink.
flush :: MonadIO m => Logger -> m ()
flush = liftIO . FL.flushLogStr . logger

-- | Closes the logger.
close :: MonadIO m => Logger -> m ()
close g = liftIO $ do
    fromMaybe (return ()) (closeDate g)
    FL.rmLoggerSet (logger g)

-- | Inspect this logger's threshold.
level :: Logger -> Level
level = logLevel . settings
{-# INLINE level #-}

putMsg :: MonadIO m => Logger -> Level -> (Msg -> Msg) -> m ()
putMsg g l f = liftIO $ do
    d <- maybe (return id) (liftM msg) (getDate g)
    let n = netstrings $ settings g
    let x = delimiter  $ settings g
    let s = nameMsg    $ settings g
    let m = render x n (d . msg (l2b l) . s . f)
    FL.pushLogStr (logger g) (FL.toLogStr m)
  where
    l2b :: Level -> ByteString
    l2b Trace = "T"
    l2b Debug = "D"
    l2b Info  = "I"
    l2b Warn  = "W"
    l2b Error = "E"
    l2b Fatal = "F"

