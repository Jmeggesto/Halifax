{-# LANGUAGE OverloadedStrings  #-}

module HTTPRequest
  (
      HTTPRequest(..)
    , HTTPResponse(..)
    , request
    , sendResponse
    , readResponse
  ) where

import Control.Applicative
import Data.Attoparsec.ByteString as P
import Data.Attoparsec.ByteString.Char8 (char8, endOfLine, isDigit_w8)
import Data.ByteString (ByteString, append)
import Data.Word (Word8)
import Data.Attoparsec.ByteString.Char8 (isEndOfLine, isHorizontalSpace)
import Data.Binary (encode)



data Header = Header {
  name  :: ByteString,
  value :: ByteString
                     } deriving (Eq, Ord, Show)

data HTTPRequest = HTTPRequest {
   method   :: ByteString,
   uri      :: ByteString,
   version  :: ByteString,
   headers  :: [Header]
                               } deriving (Eq, Ord, Show)

data HTTPResponse = HTTPResponse {
  status        :: ByteString,
  message       :: ByteString,
  body          :: ByteString
                                 } deriving (Eq, Ord, Show)

skipSpaces :: Parser ()
skipSpaces = satisfy isHorizontalSpace *> skipWhile isHorizontalSpace

isToken :: Word8 -> Bool
isToken w = w <= 127 && notInClass "\0-\31()<>@,;:\\\"/[]?={} \t" w

httpVersion :: Parser ByteString
httpVersion = "HTTP/" *> P.takeWhile (\c -> isDigit_w8 c || c == 46)

header :: Parser Header
header = Header
  <$> (P.takeWhile isToken <* char8 ':' <* skipWhile isHorizontalSpace)
  <*> (takeTill isEndOfLine <* endOfLine)


request = HTTPRequest <$> (takeWhile1 isToken <* char8 ' ')
                      <*> (takeWhile1 (/=32) <* char8 ' ')
                      <*> (httpVersion <* endOfLine)
                      <*> (many header <* many endOfLine <* endOfInput)

sendResponse :: ByteString -> ByteString -> ByteString -> HTTPResponse
sendResponse s m b = HTTPResponse s m b

readResponse :: HTTPResponse -> ByteString
readResponse res = "HTTP/1.1 " `append` (status res) `append` " " `append` (message res) `append` "\n\n" `append` (body res)

