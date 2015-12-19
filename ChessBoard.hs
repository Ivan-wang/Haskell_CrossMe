module ChessBoard (
    vHeader,
    hHeader,
    goldenMosaic,
    userMosaic,
    initChessBoard,
    loadChessBoard,
    switchLocation,
    getMosaicRow,
    getMosaicCol,
    checkMosaic,
    hint,
    cleanMosaic) where

import System.IO as S
import System.Random as R
import qualified Data.ByteString as B
import Data.Matrix
import CXM
import Status
import Utils

data ChessBoard = ChessBoard {
  vHeader :: Matrix Int,
  hHeader :: Matrix Int,
  goldenMosaic :: Matrix Status,
  userMosaic :: Matrix Status
} deriving (Show)

initChessBoard :: ChessBoard
initChessBoard = ChessBoard {
    vHeader = zero 1 1,
    hHeader = zero 1 1,
    goldenMosaic = fromList 1 1 [Unknown],
    userMosaic = fromList 1 1 [Unknown]
}

loadChessBoard :: CXM -> ChessBoard
loadChessBoard cxm = ChessBoard {
    vHeader = fromList (vAuxRow cxm) (vAuxCol cxm) ((bStr2IntList . vAuxBytes) cxm),
    hHeader = fromList (hAuxRow cxm) (hAuxCol cxm) ((bStr2IntList . hAuxBytes) cxm),
    goldenMosaic = fromList (vAuxRow cxm) (hAuxCol cxm) status,
    userMosaic = matrix (vAuxRow cxm) (hAuxCol cxm) (\(_, _) -> (Unknown::Status))
} where
    status = ((bStr2StatusList s u) . bodyBytes) cxm
    s = setFlag cxm
    u = unSetFlag cxm

loadChessBoard' :: [Int] -> [Int] -> [Status] -> ChessBoard
loadChessBoard' (vr:vc:vs) (hr:hc:hs) ms = 
    ChessBoard {
        vHeader = fromList vr vc vs,
        hHeader = fromList hr hc hs,
        goldenMosaic = fromList vr hc ms,
        userMosaic = matrix vr hc (\(_, _) -> (Unknown::Status))
    }

hint :: IO ChessBoard -> IO ChessBoard
hint ioCB = do
    cb <- ioCB
    let m = (nrows . userMosaic) cb
    r <- randomRIO (1, m)
    case compareLine cb r of
        True -> hint ioCB
        False -> return (getMosaicLine r cb) 


compareLine :: ChessBoard -> Int -> Bool
compareLine c n
    | n `mod` 2 == 1 = compareRow c (n `div` 2 + 1)
    | otherwise = compareCol c (n `div` 2)

compareRow :: ChessBoard -> Int -> Bool
compareRow c n = (((getRow n) . userMosaic) c) == (((getRow n) . goldenMosaic) c)

compareCol :: ChessBoard -> Int -> Bool
compareCol c n = (((getCol n) . userMosaic) c) == (((getCol n) . goldenMosaic) c)

switchLocation :: Int -> Int -> ChessBoard -> ChessBoard
switchLocation row col d
    | not ((isValid) row col d) = d
    | otherwise = ChessBoard {
                                vHeader = vHeader d,
                                hHeader = hHeader d,
                                goldenMosaic = goldenMosaic d,
                                userMosaic = setElem (switch oldVal) (row, col) oldMosaic
                                } where
                                    oldMosaic = userMosaic d
                                    oldVal = getElem row col $ userMosaic d

getMosaicLine :: Int -> ChessBoard -> ChessBoard
getMosaicLine n c
    | n `mod` 2 == 1 = getMosaicRow (n `div` 2 + 1) c
    | otherwise = getMosaicCol (n `div` 2) c

getMosaicCol :: Int -> ChessBoard -> ChessBoard
getMosaicCol col d 
    | not (isInRange 1 ((ncols . userMosaic) d) col) = d
    | otherwise = ChessBoard {                        
                        vHeader = vHeader d,
                        hHeader = hHeader d,
                        goldenMosaic = goldenMosaic d,
                        userMosaic = changeRow oldUserMosaic
                        } where
                            oldUserMosaic = userMosaic d
                            changeRow = mapCol goldenCol col
                            goldenCol = \row _ -> getElem row col $ goldenMosaic d

getMosaicRow :: Int -> ChessBoard -> ChessBoard
getMosaicRow row d 
    | not (isInRange 1 ((nrows . userMosaic) d) row) = d
    | otherwise = ChessBoard {
                        vHeader = vHeader d,
                        hHeader = hHeader d,
                        goldenMosaic = goldenMosaic d,
                        userMosaic = changeRow oldUserMosaic
                        } where
                            oldUserMosaic = userMosaic d
                            changeRow = mapRow goldenRow row
                            goldenRow = \col _ -> getElem row col $ goldenMosaic d

checkMosaic :: ChessBoard -> Bool
checkMosaic d = goldenMosaic d == (userMosaic d)

cleanMosaic :: ChessBoard -> ChessBoard
cleanMosaic d = ChessBoard {
                        vHeader = vHeader d,
                        hHeader = hHeader d,
                        goldenMosaic = goldenMosaic d,
                        userMosaic = newUserMosaic
                        } where
                            newUserMosaic = matrix row col (\(_, _) -> Unknown)
                            row = nrows $ goldenMosaic d
                            col = ncols $ goldenMosaic d

isInRange :: Int -> Int -> Int -> Bool
isInRange start end pos = pos `elem` [start .. end]

isValid :: Int -> Int ->ChessBoard -> Bool
isValid x y c = (isInRange 1 colNum x) && (isInRange 1 rowNum y)
    where
        colNum = 1 + ((ncols . userMosaic) c)
        rowNum = 1 + ((nrows . userMosaic) c)

-----------------test of load-------------------------

testOfLoad = do
    inh <- S.openBinaryFile "u.cxm" ReadMode
    instr <- B.hGetContents inh
    hClose inh
    case parse parseCMX instr of
        Right c -> return (Just( loadChessBoard c))
        Left err -> do
            S.putStrLn err
            return (Nothing)
