module Utils (
    w2c,
    w2i,
    i2w,
    char2Status,
    bStr2IntList,
    status2char,
    bStr2StatusList,
    statusList2bStr,
    intList2bStr
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS
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

status2char :: Char -> Char -> Status -> Char
status2char set unset s
    | s == Set = set
    | otherwise = unset

bStr2StatusList :: Char -> Char -> B.ByteString -> [Status]
bStr2StatusList set unset s = case B.uncons s of
    Nothing -> []
    Just (b, r) -> ((char2Status set unset) . w2c) b : (bStr2StatusList set unset r)

statusList2bStr :: Char -> Char -> [Status] -> B.ByteString
statusList2bStr set unset [] = B8.empty
statusList2bStr set unset (x:xs) = B.snoc (statusList2bStr set unset xs) $
                                        (BS.c2w . (status2char set unset)) x

intList2bStr :: [Int] -> B.ByteString
intList2bStr = B.pack . (map i2w)

--toolkit functions
w2c :: W.Word8 -> Char
w2c = C.chr . fromIntegral

w2i :: W.Word8 -> Int
w2i x = fromIntegral(x)::Int

i2w :: Int -> W.Word8
i2w x = fromIntegral(x)::W.Word8