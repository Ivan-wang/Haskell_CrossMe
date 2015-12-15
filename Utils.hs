module Utils (
    w2c,
    w2i
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Word as W
import Data.Char as C
import Status

bStr2IntList :: B.ByteString -> [Int]
bStr2IntList s = case B.uncons s of
    Nothing -> []
    Just (b, r) -> (w2i b) : (bStr2IntList r)

char2Status :: Char -> Char -> Char -> Status
char2Status set unset c
    | set == c = Set
    | unset == c = Unset
    | otherwise = Unknown

bStr2StatusList :: Char -> Char -> B.ByteString -> [Status]
bStr2StatusList set unset s = case B.uncons s of
    Nothing -> []
    Just (b, r) -> ((char2Status set unset) . w2c) b : (bStr2StatusList set unset r)
--toolkit functions
w2c :: W.Word8 -> Char
w2c = C.chr . fromIntegral

w2i :: W.Word8 -> Int
w2i x = fromIntegral(x)::Int