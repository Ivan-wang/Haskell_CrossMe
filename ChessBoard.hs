module ChessBoard (
    vHeader,
    hHeader,
    goldenMosaic,
    userMosaic,
    loadChessBoard,
    switchLocation,
    getMosaicRow,
    getMosaicCol,
    checkMosaic,
    cleanMosaic) where

import Data.Matrix
import Status

data ChessBoard = ChessBoard {
  vHeader :: Matrix Int,
  hHeader :: Matrix Int,
  goldenMosaic :: Matrix Status,
  userMosaic :: Matrix Status
}

loadChessBoard :: [Int] -> [Int] -> [Status] -> ChessBoard
loadChessBoard (vr:vc:vs) (hr:hc:hs) ms = 
    ChessBoard {
        vHeader = fromList vr vc vs,
        hHeader = fromList hr hc hs,
        goldenMosaic = fromList vr hc ms,
        userMosaic = matrix vr hc (\(_, _) -> (Unknown::Status))
    }

switchLocation :: Int -> Int -> ChessBoard -> ChessBoard
switchLocation row col d = ChessBoard {
                                vHeader = oldVHeader,
                                hHeader = oldHHeader,
                                goldenMosaic = oldGoldenMosaic,
                                userMosaic = setElem (switch oldVal) (row, col) oldMosaic
                                } where
                                    oldVHeader = vHeader d
                                    oldHHeader = hHeader d
                                    oldGoldenMosaic = goldenMosaic d
                                    oldMosaic = userMosaic d
                                    oldVal = getElem row col $ userMosaic d

getMosaicCol :: Int -> ChessBoard -> ChessBoard
getMosaicCol col d = ChessBoard {
                        vHeader = oldVHeader,
                        hHeader = oldHHeader,
                        goldenMosaic = oldGoldenMosaic,
                        userMosaic = changeRow oldUserMosaic
                        } where
                            oldVHeader = vHeader d
                            oldHHeader = hHeader d
                            oldGoldenMosaic = goldenMosaic d
                            oldUserMosaic = userMosaic d
                            changeRow = mapCol goldenCol col
                            goldenCol = \row _ -> getElem row col $ goldenMosaic d

getMosaicRow :: Int -> ChessBoard -> ChessBoard
getMosaicRow row d = ChessBoard {
                        vHeader = oldVHeader,
                        hHeader = oldHHeader,
                        goldenMosaic = oldGoldenMosaic,
                        userMosaic = changeRow oldUserMosaic
                        } where
                            oldVHeader = vHeader d
                            oldHHeader = hHeader d
                            oldGoldenMosaic = goldenMosaic d
                            oldUserMosaic = userMosaic d
                            changeRow = mapRow goldenRow row
                            goldenRow = \col _ -> getElem row col $ goldenMosaic d

checkMosaic :: ChessBoard -> Bool
checkMosaic d = goldenMosaic d == (userMosaic d)

cleanMosaic :: ChessBoard -> ChessBoard
cleanMosaic d = ChessBoard {
                        vHeader = oldVHeader,
                        hHeader = oldHHeader,
                        goldenMosaic = oldGoldenMosaic,
                        userMosaic = newUserMosaic
                        } where
                            oldVHeader = vHeader d
                            oldHHeader = hHeader d
                            oldGoldenMosaic = goldenMosaic d
                            newUserMosaic = matrix row col (\(_, _) -> Unknown)
                            row = nrows $ goldenMosaic d
                            col = ncols $ goldenMosaic d