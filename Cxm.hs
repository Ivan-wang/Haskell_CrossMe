module CXM (
    CXM(..),
    parse,
    parseCMX,
    loadFromFile,
    serialize,
    exportToFile) where


import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Word as W
import qualified Control.Applicative
import Control.Monad
import System.IO as S
import Utils

data CXM = CXM {
    vAuxRow :: Int,
    vAuxCol :: Int,
    hAuxRow :: Int,
    hAuxCol :: Int,
    setFlag :: Char,
    unSetFlag :: Char,
    cxmName :: String,
    vAuxBytes :: B.ByteString,
    hAuxBytes :: B.ByteString,
    bodyBytes :: B.ByteString
} deriving (Show)

data ParseState = ParseState {
    remain :: B.ByteString
} deriving (Show)

--Parser is a wrapper of a function
--runParser unpack the function
newtype Parser a = Parser {
    runParser :: ParseState -> Either String (a, ParseState)
}

--Apply f to parser result
--run a parser, get the result then do some thing
--after that it injects result of function in to new state
--the ParserState is pass to new parser in background
instance Functor Parser where
    fmap f parser = parser ==> \result -> identity (f result)

instance Applicative Parser where
    pure = identity
    (<*>) = ap

instance Monad Parser where
    return = identity
    (>>=) = (==>)
    (>>) = (==>&)

--identity generate a parser that do not modify state
--an injector, which inject a value in parser result
identity :: a -> Parser a
identity a = Parser (\s -> Right (a, s))

--Chain the depended parser together
--unpack result from first parser then generate a new parser
(==>) :: Parser a -> (a -> Parser b) -> Parser b
fstParser ==> sndParser = Parser newParser
    where newParser = (\initS -> case runParser fstParser initS of
                                    Left err -> Left err
                                    Right (medium, newS) -> 
                                        runParser (sndParser medium) newS)
--chain independ parser together
(==>&) :: Parser a -> Parser b -> Parser b
fstParser ==>& sndParser = fstParser ==> \_ -> sndParser

getState :: Parser ParseState
getState = Parser (\s -> Right (s, s))

putState :: ParseState -> Parser ()
putState s = Parser (\_ -> Right((), s))

---------------------------Concrete Parsers------------------------------

peekByte :: Parser (Maybe W.Word8)
peekByte = (fmap fst . B.uncons . remain) <$> getState

parseByte :: Parser W.Word8
parseByte = 
    getState ==> \initS -> 
    case B.uncons (remain initS) of 
        Nothing -> Parser (\_ -> Left "No more byte")
        Just (byte, r) -> 
            putState newState ==> \_ -> identity byte
            where newState = ParseState (r)

parseChar :: Parser Char
parseChar = w2c <$> parseByte

parseInt :: Parser Int
parseInt = w2i <$> parseByte

matchByte :: (W.Word8 -> Bool) -> Parser ()
matchByte f = (fmap f <$> peekByte) ==> \isM -> 
                    if isM == Just True
                    then parseByte ==> (\_ -> identity ())
                    else Parser(\_ -> Left "Not match")

matchChar :: (Char -> Bool) -> Parser ()
matchChar f = (fmap (f . w2c) <$> peekByte) ==> \isM -> 
                    if isM == Just True
                    then parseByte ==> (\_ -> identity ())
                    else Parser(\_ -> Left "Not match")

matchPrefix :: [Char] -> Parser ()
matchPrefix [] = identity ()
matchPrefix (c:cs) = (matchChar (== c)) ==> \_ -> matchPrefix cs

parseString :: Parser String
parseString = do
    mp <- (fmap ((== ' ') . w2c) <$> peekByte)
    if mp == Just False
    then do
        b <- parseByte
        ((w2c b) :) <$> parseString
    else identity []


skipSpace :: Parser ()
skipSpace = (fmap ((== ' ') . w2c) <$> peekByte) ==> \isM -> 
                    if isM == Just True
                    then parseByte ==> (\_ -> skipSpace)
                    else identity ()

_dropBlock :: Int -> Parser ()
_dropBlock l = getState ==> \inits -> putState $ ParseState (B.drop l $ remain inits)

parseBlock :: Int -> Parser B.ByteString
parseBlock l = (((B.take l) . remain) <$> getState) ==> \list ->
                if (B.length list) == l
                then (_dropBlock l) ==> \_ -> identity list
                else Parser (\_ -> Left "block size not match")

parseCMX :: Parser CXM
parseCMX = (matchPrefix "CXM")  ==> \header -> skipSpace                 ==>&
            (matchPrefix "N") ==> \nHeader -> skipSpace                  ==>&
            parseString ==> \name -> skipSpace                           ==>&
            (matchPrefix "H")   ==> \hHeader -> skipSpace                ==>&
            parseInt    ==> \hRow -> parseInt   ==> \hCol -> skipSpace   ==>& 
            (parseBlock (hRow * hCol))  ==> \hBlock -> skipSpace         ==>&
            (matchPrefix "V")   ==> \vHeader -> skipSpace                ==>&
            parseInt    ==> \vRow -> parseInt   ==> \vCol -> skipSpace   ==>&
            (parseBlock (vRow * vCol))  ==> \vBlock -> skipSpace         ==>& 
            (matchPrefix "B")   ==> \bHeader -> skipSpace                ==>&
            parseInt    ==> \bRow -> parseInt   ==> \bCol -> skipSpace   ==>&
            parseChar   ==> \sC -> parseChar    ==> \uC -> skipSpace     ==>&
            (parseBlock (bRow * bCol)) ==> 
            (\bBlock -> identity (CXM vRow vCol hRow hCol sC uC name vBlock hBlock bBlock))

--------------NEW A CXM FROM USER---------------

serialize :: CXM -> B.ByteString
serialize cxm = B.concat [B8.pack "CXM", 
    B8.pack " N",
    (B8.pack . cxmName) cxm,
    B8.pack " H",
    (B.singleton . i2w . hAuxRow) cxm,
    (B.singleton . i2w . hAuxCol) cxm,
    B8.pack " ",
    vAuxBytes cxm,
    B8.pack " V",
    (B.singleton . i2w . vAuxRow) cxm,
    (B.singleton . i2w . vAuxCol) cxm,
    hAuxBytes cxm,
    B8.pack " B",
    (B.singleton . i2w . vAuxRow) cxm,
    (B.singleton . i2w . hAuxCol) cxm,
    B8.pack " SR ",
    bodyBytes cxm,
    B8.empty]

-------------IMPORT AND EXPORT----------------

loadFromFile :: String -> IO (Maybe CXM)
loadFromFile fileName = do
    inh <- openBinaryFile fileName ReadMode
    instr <- B.hGetContents inh
    hClose inh
    case parse parseCMX instr of
        Right c -> return (Just c)
        Left err -> do
            S.putStrLn err
            return (Nothing)

exportToFile :: String -> CXM -> IO ()
exportToFile fileName cmxObj = do
    inh <- openBinaryFile fileName WriteMode
    B.hPut inh (serialize cmxObj)
    hClose inh


-------------To test a parser--------------

parse :: Parser a -> B.ByteString -> Either String a
parse parser initS
    = case runParser parser (ParseState initS) of
        Left err -> Left err
        Right (r, _) -> Right r


main = 
    do
        inh <- openBinaryFile "u.cxm" ReadMode
        instr <- B.hGetContents inh
        hClose inh
        return (parse parseCMX instr)

