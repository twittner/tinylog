{-# LANGUAGE OverloadedStrings #-}

module System.Logger.Message where

import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Monoid

type Builder = Msg -> Msg

newtype Msg = Msg
    { parts :: [ByteString]
    } deriving (Eq, Ord, Show)

empty :: Msg
empty = Msg []

set :: ByteString -> Builder
set b = const $ Msg [b]

msg :: ByteString -> Builder
msg p (Msg m) = Msg (p:m)

msg' :: ByteString -> Builder
msg' ""     m  = m
msg' p (Msg m) = Msg (p:m)

field :: ByteString -> ByteString -> Builder
field k v (Msg m) = Msg (k <> "=" <> v : m)

render :: ByteString -> Builder -> ByteString
render s f = mconcat . intersperse s . parts $ f empty
