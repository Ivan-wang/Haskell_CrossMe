module ChessBoard (
    vHeader,
    hHeader,
    name,
    goldenMosaic,
    userMosaic,
    initChessBoard,
    loadChessBoard,
    switchLocation,
    newChessBoard,
    saveChessBoard,
    getMosaicRow,
    getMosaicCol,
    checkMosaic,
    expandChessBoard,
    hint,
    cleanMosaic) where

import System.IO as S
import System.Random as R
import qualified Data.ByteString as B
import qualified Data.List as L
import Data.Matrix
import CXM
import Status
import Utils

data ChessBoard = ChessBoard {
  name :: String,
  vHeader :: Matrix Int,
  hHeader :: Matrix Int,
  goldenMosaic :: Matrix Status,
  userMosaic :: Matrix Status
} deriving (Show)

initChessBoard :: ChessBoard
initChessBoard = ChessBoard {
    name = "",
    vHeader = zero 0 0,
    hHeader = zero 0 0,
    goldenMosaic = fromList 0 0 [Unknown],
    userMosaic = fromList 0 0 [Unknown]
}

newChessBoard :: Int -> Int -> ChessBoard
newChessBoard row col = ChessBoard {
    name = "",
    vHeader = zero 0 0,
    hHeader = zero 0 0,
    goldenMosaic = fromList 0 0 [Unknown],
    userMosaic = matrix row col (\(_, _) -> Unknown)
}

saveChessBoard :: ChessBoard -> CXM
saveChessBoard cb = CXM {
    vAuxRow = (nrows . vHeader) cb,
    vAuxCol = (ncols . vHeader) cb,
    hAuxRow = (nrows . hHeader) cb,
    hAuxCol = (ncols . hHeader) cb,
    setFlag = 'S',
    unSetFlag = 'R',
    cxmName = name cb,
    vAuxBytes = (intList2bStr . toList . vHeader) cb,
    hAuxBytes = (intList2bStr . toList . hHeader) cb,
    bodyBytes = ((statusList2bStr 'S' 'R') . toList . goldenMosaic) cb
}

loadChessBoard :: CXM -> ChessBoard
loadChessBoard cxm = ChessBoard {
    name = cxmName cxm,
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
        name = "",
        vHeader = fromList vr vc vs,
        hHeader = fromList hr hc hs,
        goldenMosaic = fromList vr hc ms,
        userMosaic = matrix vr hc (\(_, _) -> (Unknown::Status))
    }

hint :: IO ChessBoard -> IO ChessBoard
hint ioCB = do
    cb <- ioCB
    let m = (nrows . userMosaic) cb
        n = (ncols . userMosaic) cb
    r <- randomRIO (1, m+n)
    case compareLine cb r of
        True -> hint ioCB
        False -> return (getMosaicLine r cb) 


compareLine :: ChessBoard -> Int -> Bool
compareLine c n
    | n <= ((nrows . userMosaic) c)= compareRow c n
    | otherwise = compareCol c (n - ((nrows . userMosaic) c))

compareRow :: ChessBoard -> Int -> Bool
compareRow c n = (((getRow n) . userMosaic) c) == (((getRow n) . goldenMosaic) c)

compareCol :: ChessBoard -> Int -> Bool
compareCol c n = (((getCol n) . userMosaic) c) == (((getCol n) . goldenMosaic) c)

switchLocation :: Int -> Int -> ChessBoard -> ChessBoard
switchLocation row col d
    | not ((isValid) col row d) = d
    | otherwise = ChessBoard {
                                name = name d,
                                vHeader = vHeader d,
                                hHeader = hHeader d,
                                goldenMosaic = goldenMosaic d,
                                userMosaic = setElem (switch oldVal) (row, col) oldMosaic
                                } where
                                    oldMosaic = userMosaic d
                                    oldVal = getElem row col $ userMosaic d

getMosaicLine :: Int -> ChessBoard -> ChessBoard
getMosaicLine n c
    | n <= ((nrows . userMosaic) c) = getMosaicRow n c
    | otherwise = getMosaicCol (n - ((nrows . userMosaic) c)) c

getMosaicCol :: Int -> ChessBoard -> ChessBoard
getMosaicCol col d 
    | not (isInRange 1 ((ncols . userMosaic) d) col) = d
    | otherwise = ChessBoard {                        
                        name = name d,
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
                        name = name d,
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
                        name = name d,
                        vHeader = vHeader d,
                        hHeader = hHeader d,
                        goldenMosaic = goldenMosaic d,
                        userMosaic = newUserMosaic
                        } where
                            newUserMosaic = matrix row col (\(_, _) -> Unknown)
                            row = nrows $ goldenMosaic d
                            col = ncols $ goldenMosaic d

-------------Toolkit functions----------------

--This function expand a partial chess board to a entire one
--partial chess board must have a name and a userMosaic
--and this function will rebuild the headers according to the userMosaic
expandChessBoard :: ChessBoard -> ChessBoard
expandChessBoard raw = ChessBoard {
    name = name raw,
    vHeader = summaryMtrix (userMosaic raw),
    hHeader = summaryMtrix ((transpose . userMosaic) raw),
    goldenMosaic = userMosaic raw,
    userMosaic = matrix row col (\(_, _) -> Unknown)
} where
    row = (nrows . userMosaic) raw
    col = (ncols . userMosaic) raw


summary :: [Status] -> [Int]
summary l = map length $ filter ((== Set) . head) $ L.group l

allignSummary :: Int -> [Int] -> [Int]
allignSummary len l
    | length l == len = l
    | otherwise = (replicate (len - (length l)) 0) ++ l 

summaryMtrix :: Matrix Status -> Matrix Int
summaryMtrix m = fromLists (map (allignSummary maxL) s) where
    s = map summary $ toLists m
    maxL = maximum (map length s)

isInRange :: Int -> Int -> Int -> Bool
isInRange start end pos = pos `elem` [start .. end]

isValid :: Int -> Int ->ChessBoard -> Bool
isValid x y c = (isInRange 1 colNum x) && (isInRange 1 rowNum y)
    where
        colNum = (ncols . userMosaic) c
        rowNum = (nrows . userMosaic) c


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
