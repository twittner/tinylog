-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

-- | Small layer on top of @fast-logger@ which adds log-levels and
-- timestamp support and not much more.
module System.Logger
    ( Level    (..)
    , Output   (..)
    , Settings (..)
    , Logger
    , DateFormat

    , new
    , create
    , defSettings
    , level
    , flush
    , close

    , log
    , trace
    , debug
    , info
    , warn
    , err
    , fatal

    , iso8601UTC
    , module M
    )
where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.String
import Data.UnixTime
import System.Date.Cache
import System.Environment (lookupEnv)
import System.Log.FastLogger (BufSize)
import System.Logger.Message as M

import qualified System.Log.FastLogger as FL

data Level
    = Trace
    | Debug
    | Info
    | Warn
    | Error
    | Fatal
    deriving (Eq, Ord, Read, Show)

data Logger = Logger
    { _logger    :: FL.LoggerSet
    , _settings  :: Settings
    , _getDate   :: Maybe DateCacheGetter
    , _closeDate :: Maybe DateCacheCloser
    }

data Settings = Settings
    { logLevel  :: Level      -- ^ messages below this log level will be suppressed
    , output    :: Output     -- ^ log sink
    , format    :: DateFormat -- ^ the timestamp format (use \"\" to disable timestamps)
    , delimiter :: ByteString -- ^ text to intersperse between fields of a log line
    , bufSize   :: BufSize    -- ^ how many bytes to buffer before commiting to sink
    } deriving (Eq, Ord, Show)

data Output
    = StdOut
    | StdErr
    | Path FilePath
    deriving (Eq, Ord, Show)

newtype DateFormat = DateFormat
    { template :: ByteString
    } deriving (Eq, Ord, Show)

instance IsString DateFormat where
    fromString = DateFormat . pack

iso8601UTC :: DateFormat
iso8601UTC = "%Y-%0m-%0dT%0H:%0M:%0SZ"

-- | Default settings for use with 'new':
--
--   * 'logLevel'  = 'Debug'
--
--   * 'output'    = 'StdOut'
--
--   * 'format'    = 'iso8601UTC'
--
--   * 'delimiter' = \", \"
--
--   * 'bufSize'   = 'FL.defaultBufSize'
--
defSettings :: Settings
defSettings = Settings Debug StdOut iso8601UTC ", " FL.defaultBufSize

-- | Create a new 'Logger' with the given 'Settings'.
-- Please note that the 'logLevel' can be dynamically adjusted by setting
-- the environment variable @LOG_LEVEL@ accordingly. Likewise the buffer
-- size can be dynamically set via @LOG_BUFFER@.
new :: MonadIO m => Settings -> m Logger
new s = liftIO $ do
    n <- fmap (readNote "Invalid LOG_BUFFER") <$> lookupEnv "LOG_BUFFER"
    l <- fmap (readNote "Invalid LOG_LEVEL")  <$> lookupEnv "LOG_LEVEL"
    g <- fn (output s) (fromMaybe (bufSize s) n)
    c <- clockCache (format s)
    let s' = s { logLevel = fromMaybe (logLevel s) l }
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

-- | Logs a message with the given level if greater of equal to the
-- logger's threshold.
log :: MonadIO m => Logger -> Level -> (Msg -> Msg) -> m ()
log g l m = unless (level g > l) . liftIO $ putMsg g l m
{-# INLINE log #-}

-- | Abbreviation for 'log' using the corresponding log level.
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

-- | Force buffered bytes to output sink.
flush :: MonadIO m => Logger -> m ()
flush = liftIO . FL.flushLogStr . _logger

-- | Closes the logger.
close :: MonadIO m => Logger -> m ()
close g = liftIO $ do
    fromMaybe (return ()) (_closeDate g)
    FL.rmLoggerSet (_logger g)

-- | Inspect this logger's threshold.
level :: Logger -> Level
level = logLevel . _settings
{-# INLINE level #-}

putMsg :: MonadIO m => Logger -> Level -> (Msg -> Msg) -> m ()
putMsg g l f = liftIO $ do
    d <- maybe (return id) (liftM msg) (_getDate g)
    let m = render (delimiter $ _settings g) (d . msg (l2b l) . f)
    FL.pushLogStr (_logger g) (FL.toLogStr m)
  where
    l2b :: Level -> ByteString
    l2b Trace = "T"
    l2b Debug = "D"
    l2b Info  = "I"
    l2b Warn  = "W"
    l2b Error = "E"
    l2b Fatal = "F"

