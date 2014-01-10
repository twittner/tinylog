-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module System.Logger.Message
    ( Builder
    , Msg
    , msg
    , field
    , render
    ) where

import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Monoid

import qualified Data.ByteString.Lazy                as L
import qualified Data.ByteString.Lazy.Builder        as B
import qualified Data.ByteString.Lazy.Builder.Extras as B

type Builder = Msg -> Msg
newtype Msg  = Msg { builders :: [B.Builder] }

msg :: ByteString -> Builder
msg p (Msg m) = Msg (B.byteString p : m)

field :: ByteString -> ByteString -> Builder
field k v (Msg m) = Msg $
    B.byteString k <> B.byteString "=" <> B.byteString v : m

render :: ByteString -> Builder -> L.ByteString
render s f = finish
           . mconcat
           . intersperse (B.byteString s)
           . builders
           . f
           $ empty
  where
    finish = B.toLazyByteStringWith (B.untrimmedStrategy 128 256) "\n"
    empty  = Msg []

