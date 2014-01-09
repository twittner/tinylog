{-# LANGUAGE OverloadedStrings #-}

module System.Logger.Message
    ( Builder
    , Msg
    , value
    , msg
    , msg'
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

value :: ByteString -> Builder
value b = const $ Msg [B.byteString b]

msg :: ByteString -> Builder
msg p (Msg m) = Msg (B.byteString p : m)

msg' :: ByteString -> Builder
msg' ""     m  = m
msg' p (Msg m) = Msg (B.byteString p : m)

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

