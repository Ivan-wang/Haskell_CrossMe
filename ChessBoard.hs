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
    cleanMosaic) where

import System.IO as S
import Data.ByteString as B
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

--fillChessBoard :: CXM -> ChessBoard -> ChessBoard
--fillChessBoard cxm ch = 

loadChessBoard :: CXM -> ChessBoard
loadChessBoard cxm = ChessBoard {
    vHeader = fromList (vAuxRow cxm) (vAuxCol cxm) ((bStr2IntList . vAuxBytes) cxm),
    hHeader = fromList (hAuxRow cxm) (hAuxCol cxm) ((bStr2IntList . hAuxBytes) cxm),
    goldenMosaic = fromList (hAuxRow cxm) (vAuxCol cxm) status,
    userMosaic = matrix (hAuxRow cxm) (vAuxCol cxm) (\(_, _) -> (Unknown::Status))
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

switchLocation :: Int -> Int -> ChessBoard -> ChessBoard
switchLocation row col d = ChessBoard {
                                vHeader = vHeader d,
                                hHeader = hHeader d,
                                goldenMosaic = goldenMosaic d,
                                userMosaic = setElem (switch oldVal) (row, col) oldMosaic
                                } where
                                    oldMosaic = userMosaic d
                                    oldVal = getElem row col $ userMosaic d

getMosaicCol :: Int -> ChessBoard -> ChessBoard
getMosaicCol col d = ChessBoard {                        
                        vHeader = vHeader d,
                        hHeader = hHeader d,
                        goldenMosaic = goldenMosaic d,
                        userMosaic = changeRow oldUserMosaic
                        } where
                            oldUserMosaic = userMosaic d
                            changeRow = mapCol goldenCol col
                            goldenCol = \row _ -> getElem row col $ goldenMosaic d

getMosaicRow :: Int -> ChessBoard -> ChessBoard
getMosaicRow row d = ChessBoard {
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
