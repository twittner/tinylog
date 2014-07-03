-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module System.Logger.Settings where

import Data.String
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Text (Text)
import System.Log.FastLogger (defaultBufSize)
import System.Logger.Message

data Settings = Settings
    { logLevel   :: !Level      -- ^ messages below this log level will be suppressed
    , output     :: !Output     -- ^ log sink
    , format     :: !DateFormat -- ^ the timestamp format (use \"\" to disable timestamps)
    , delimiter  :: !ByteString -- ^ text to intersperse between fields of a log line
    , netstrings :: !Bool       -- ^ use <http://cr.yp.to/proto/netstrings.txt netstrings> encoding (fixes delimiter to \",\")
    , bufSize    :: !Int        -- ^ how many bytes to buffer before commiting to sink
    , name       :: !Text       -- ^ logger name
    , nameMsg    :: Msg -> Msg
    }

setOutput :: Output -> Settings -> Settings
setOutput x s = s { output = x }

setFormat :: DateFormat -> Settings -> Settings
setFormat x s = s { format = x }

setBufSize :: Int -> Settings -> Settings
setBufSize x s = s { bufSize = max 1 x }

setDelimiter :: ByteString -> Settings -> Settings
setDelimiter x s = s { delimiter = x }

setNetStrings :: Bool -> Settings -> Settings
setNetStrings x s = s { netstrings = x }

setLogLevel :: Level -> Settings -> Settings
setLogLevel x s = s { logLevel = x }

setName :: Text -> Settings -> Settings
setName "" s = s { name = "", nameMsg = id }
setName xs s = s { name = xs, nameMsg = "logger" .= xs }

data Level
    = Trace
    | Debug
    | Info
    | Warn
    | Error
    | Fatal
    deriving (Eq, Ord, Read, Show)

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

-- | ISO 8601 date-time format.
iso8601UTC :: DateFormat
iso8601UTC = "%Y-%0m-%0dT%0H:%0M:%0SZ"

-- | Default settings:
--
--   * 'logLevel'   = 'Debug'
--
--   * 'output'     = 'StdOut'
--
--   * 'format'     = 'iso8601UTC'
--
--   * 'delimiter'  = \", \"
--
--   * 'netstrings' = False
--
--   * 'bufSize'    = 'FL.defaultBufSize'
--
--   * 'name'       = \"\"
--
defSettings :: Settings
defSettings = Settings Debug StdOut iso8601UTC ", " False defaultBufSize "" id

