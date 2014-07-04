-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

-- | 'Msg' and 'ToBytes' assist in constructing log messages.
-- For example:
--
-- @
-- > g <- new defSettings { bufSize = 1, output = StdOut }
-- > info g $ msg "some text" ~~ "key" .= "value" ~~ "okay" .= True
-- 2014-04-28T21:18:20Z, I, some text, key=value, okay=True
-- >
-- @
module System.Logger.Message
    ( ToBytes (..)
    , Msg
    , Builder
    , msg
    , field
    , (.=)
    , (+++)
    , (~~)
    , val
    , eval
    , render
    ) where

import Data.ByteString (ByteString)
import Data.Double.Conversion.Text
import Data.Int
import Data.Monoid
import Data.String
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word
import GHC.Float

import qualified Data.ByteString                     as S
import qualified Data.ByteString.Lazy                as L
import qualified Data.ByteString.Lazy.Builder        as B
import qualified Data.ByteString.Lazy.Builder.Extras as B
import qualified Data.Text                           as T
import qualified Data.Text.Lazy                      as TL
import qualified Data.Text.Lazy.Encoding             as TL

data Builder = Builder !Int B.Builder

instance Monoid Builder where
    mempty = Builder 0 mempty
    (Builder x a) `mappend` (Builder y b) = Builder (x + y) (a <> b)

instance IsString Builder where
    fromString = bytes

eval :: Builder -> L.ByteString
eval (Builder n b) = B.toLazyByteStringWith (B.safeStrategy n 256) L.empty b

-- | Convert some value to a 'Builder'.
class ToBytes a where
    bytes :: a -> Builder

instance ToBytes Builder      where bytes x = x
instance ToBytes L.ByteString where bytes x = Builder (fromIntegral $ L.length x) (B.lazyByteString x)
instance ToBytes ByteString   where bytes x = Builder (S.length x) (B.byteString x)
instance ToBytes Int          where bytes x = Builder (len10 x) (B.intDec x)
instance ToBytes Int8         where bytes x = Builder (len10 x) (B.int8Dec x)
instance ToBytes Int16        where bytes x = Builder (len10 x) (B.int16Dec x)
instance ToBytes Int32        where bytes x = Builder (len10 x) (B.int32Dec x)
instance ToBytes Int64        where bytes x = Builder (len10 x) (B.int64Dec x)
instance ToBytes Integer      where bytes x = Builder (len10 x) (B.integerDec x)
instance ToBytes Word         where bytes x = Builder (len10 x) (B.wordDec x)
instance ToBytes Word8        where bytes x = Builder (len10 x) (B.word8Dec x)
instance ToBytes Word16       where bytes x = Builder (len10 x) (B.word16Dec x)
instance ToBytes Word32       where bytes x = Builder (len10 x) (B.word32Dec x)
instance ToBytes Word64       where bytes x = Builder (len10 x) (B.word64Dec x)
instance ToBytes Float        where bytes x = bytes (toShortest $ float2Double x)
instance ToBytes Double       where bytes x = bytes (toShortest x)
instance ToBytes Text         where bytes x = bytes (encodeUtf8 x)
instance ToBytes TL.Text      where bytes x = bytes (TL.encodeUtf8 x)
instance ToBytes Char         where bytes x = bytes (T.singleton x)
instance ToBytes [Char]       where bytes x = bytes (TL.pack x)

instance ToBytes Bool where
    bytes True  = Builder 4 (B.byteString "True")
    bytes False = Builder 5 (B.byteString "False")

{-# INLINE len10 #-}
len10 :: Integral a => a -> Int
len10 !n = if n > 0 then go n 0 else 1 + go (-n) 0
  where
    go  0 !a = a
    go !x !a = go (x `div` 10) (a + 1)

-- | Type representing log messages.
newtype Msg = Msg { elements :: [Element] }

data Element
    = Bytes Builder
    | Field Builder Builder

-- | Turn some value into a 'Msg'.
msg :: ToBytes a => a -> Msg -> Msg
msg p (Msg m) = Msg $ Bytes (bytes p) : m

-- | Render some field, i.e. a key-value pair delimited by \"=\".
field :: ToBytes a => ByteString -> a -> Msg -> Msg
field k v (Msg m) = Msg $ Field (bytes k) (bytes v) : m

-- | Alias of 'field'.
(.=) :: ToBytes a => ByteString -> a -> Msg -> Msg
(.=) = field
infixr 5 .=

-- | Alias of '.' with lowered precedence to allow combination with '.='
-- without requiring parentheses.
(~~) :: (b -> c) -> (a -> b) -> a -> c
(~~) = (.)
infixr 4 ~~

-- | Concatenate two 'ToBytes' values.
(+++) :: (ToBytes a, ToBytes b) => a -> b -> Builder
a +++ b = bytes a <> bytes b
infixr 6 +++

-- | Type restriction. Useful to disambiguate string literals when
-- using @OverloadedStrings@ pragma.
val :: ByteString -> Builder
val = bytes

-- | Intersperse parts of the log message with the given delimiter and
-- render the whole builder into a 'L.ByteString'.
--
-- If the second parameter is set to @True@, netstrings encoding is used for
-- the message elements. Cf. <http://cr.yp.to/proto/netstrings.txt> for
-- details.
render :: ByteString -> Bool -> (Msg -> Msg) -> L.ByteString
render _ True m = finish . encAll mempty . elements . m $ empty
  where
    encAll !acc []     = acc
    encAll !acc (b:bb) = encAll (acc <> encOne b) bb

    encOne (Bytes   e) = netstr e
    encOne (Field k v) = netstr k <> eq <> netstr v

    eq = B.byteString "1:=,"

render s False m = finish . encAll mempty . elements . m $ empty
  where
    encAll !acc    []  = acc
    encAll !acc (b:[]) = acc <> encOne b
    encAll !acc (b:bb) = encAll (acc <> encOne b <> sep) bb

    encOne (Bytes (Builder _ b))               = b
    encOne (Field (Builder _ k) (Builder _ v)) = k <> eq <> v

    eq  = B.char8 '='
    sep = B.byteString s

finish :: B.Builder -> L.ByteString
finish = B.toLazyByteStringWith (B.untrimmedStrategy 256 256) "\n"

empty :: Msg
empty = Msg []

netstr :: Builder -> B.Builder
netstr (Builder !n b) = B.intDec n <> colon <> b <> comma

colon, comma :: B.Builder
colon = B.char8 ':'
comma = B.char8 ','
