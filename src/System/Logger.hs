-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module System.Logger
    ( Level    (..)
    , Output   (..)
    , Settings (..)
    , Logger
    , Format

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

    , logM
    , traceM
    , debugM
    , infoM
    , warnM
    , errM
    , fatalM

    , iso8601UTC
    )
where

import Prelude hiding (log)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.String
import Data.UnixTime hiding (Format)
import System.Date.Cache
import System.Environment (lookupEnv)

import qualified System.Log.FastLogger as FL

data Level
    = Trace
    | Debug
    | Info
    | Warn
    | Error
    | Fatal
    deriving (Eq, Ord, Read, Show)

l2b :: Level -> ByteString
l2b Trace = "TRACE"
l2b Debug = "DEBUG"
l2b Info  = "INFO"
l2b Warn  = "WARN"
l2b Error = "ERROR"
l2b Fatal = "FATAL"
{-# INLINE l2b #-}

data Logger = Logger
    { _logger    :: !FL.LoggerSet
    , _getDate   :: !DateCacheGetter
    , _closeDate :: !DateCacheCloser
    , _settings  :: !Settings
    }

data Settings = Settings
    { logLevel  :: !Level
    , output    :: !Output
    , format    :: !Format
    , delimiter :: !ByteString
    } deriving (Eq, Ord, Show)

data Output
    = StdOut
    | Path FilePath
    deriving (Eq, Ord, Show)

newtype Format = Format
    { template :: ByteString
    } deriving (Eq, Ord, Show)

instance IsString Format where
    fromString = Format . pack

iso8601UTC :: Format
iso8601UTC = "%Y-%0m-%0dT%0H:%0M:%0SZ"

defSettings :: Settings
defSettings = Settings Debug StdOut iso8601UTC ", "

new :: MonadIO m => Settings -> m Logger
new s = liftIO $ do
    n <- fmap (readNote "Invalid LOG_BUFFER.") <$> lookupEnv "LOG_BUFFER"
    g <- FL.newLoggerSet (fromMaybe FL.defaultBufSize n) (mapOut $ output s)
    (x, y) <- clockDateCacher $ DateCacheConf getUnixTime fmt
    return $ Logger g x y s
  where
    mapOut StdOut   = Nothing
    mapOut (Path p) = Just p

    fmt :: UnixTime -> IO ByteString
    fmt = return . formatUnixTimeGMT (template $ format s)

create :: MonadIO m => Output -> m Logger
create p = liftIO $ do
    ll <- fmap (readNote "Invalid LOG_LEVEL.") <$> lookupEnv "LOG_LEVEL"
    new defSettings { logLevel = fromMaybe Debug ll, output = p }

readNote :: Read a => String -> String -> a
readNote m s = case reads s of
    [(a, "")] -> a
    _         -> error m

log :: MonadIO m => Logger -> Level -> ByteString -> m ()
log g l m = unless (level g > l) . liftIO $ putMsg g l m
{-# INLINE log #-}

logM :: MonadIO m => Logger -> Level -> m ByteString -> m ()
logM g l m = unless (level g > l) $ m >>= putMsg g l
{-# INLINE logM #-}

trace, debug, info, warn, err, fatal :: MonadIO m => Logger -> ByteString -> m ()
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

traceM, debugM, infoM, warnM, errM, fatalM :: MonadIO m => Logger -> m ByteString -> m ()
traceM g = logM g Trace
debugM g = logM g Debug
infoM  g = logM g Info
warnM  g = logM g Warn
errM   g = logM g Error
fatalM g = logM g Fatal
{-# INLINE traceM #-}
{-# INLINE debugM #-}
{-# INLINE infoM  #-}
{-# INLINE warnM  #-}
{-# INLINE errM   #-}
{-# INLINE fatalM #-}

flush :: MonadIO m => Logger -> m ()
flush = liftIO . FL.flushLogStr . _logger

close :: MonadIO m => Logger -> m ()
close g = liftIO $ do
    _closeDate g
    FL.rmLoggerSet (_logger g)

level :: Logger -> Level
level = logLevel . _settings
{-# INLINE level #-}

putMsg :: MonadIO m => Logger -> Level -> ByteString -> m ()
putMsg g l m = liftIO $ do
    let x = delimiter $ _settings g
    d <- _getDate g
    FL.pushLogStr (_logger g) . FL.toLogStr $ mconcat [d, x, l2b l, x, m, "\n"]
{-# INLINE putMsg #-}
