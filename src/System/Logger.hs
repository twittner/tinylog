-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

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

    , logM
    , traceM
    , debugM
    , infoM
    , warnM
    , errM
    , fatalM

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
    { logLevel  :: Level
    , output    :: Output
    , format    :: DateFormat
    , delimiter :: ByteString
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

defSettings :: Settings
defSettings = Settings Debug StdOut iso8601UTC ", "

new :: MonadIO m => Settings -> m Logger
new s = liftIO $ do
    n <- fmap (readNote "Invalid LOG_BUFFER") <$> lookupEnv "LOG_BUFFER"
    l <- fmap (readNote "Invalid LOG_LEVEL")  <$> lookupEnv "LOG_LEVEL"
    g <- fn (output s) (fromMaybe FL.defaultBufSize n)
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

create :: MonadIO m => Output -> m Logger
create p = new defSettings { output = p }

readNote :: Read a => String -> String -> a
readNote m s = case reads s of
    [(a, "")] -> a
    _         -> error m

log :: MonadIO m => Logger -> Level -> Builder -> m ()
log g l m = unless (level g > l) . liftIO $ putMsg g l m
{-# INLINE log #-}

logM :: MonadIO m => Logger -> Level -> m Builder -> m ()
logM g l m = unless (level g > l) $ m >>= putMsg g l
{-# INLINE logM #-}

trace, debug, info, warn, err, fatal :: MonadIO m => Logger -> Builder -> m ()
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

traceM, debugM, infoM, warnM, errM, fatalM :: MonadIO m => Logger -> m Builder -> m ()
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
    fromMaybe (return ()) (_closeDate g)
    FL.rmLoggerSet (_logger g)

level :: Logger -> Level
level = logLevel . _settings
{-# INLINE level #-}

putMsg :: MonadIO m => Logger -> Level -> Builder -> m ()
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

