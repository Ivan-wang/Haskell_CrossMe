
import Graphics.UI.WX
import ChessBoard
import System.IO as S
import Data.ByteString as B
import Data.Matrix
import CXM
import Status
import Utils


main = do
    varC <- varCreate initChessBoard
    cmxObj <- testOfLoad
    case cmxObj of
        Nothing -> varUpdate varC (id)
        Just c -> do
            --varC <- varCreate initChessBoard
            varUpdate varC (\old -> loadChessBoard c)
    varUpdate varC (switchLocation 1 1)
    newC <- varGet varC
    --isRight <- fmap checkMosaic (varGet varC)
    case checkMosaic newC of
        True -> return (True)
        False -> return (False)


testOfLoad = do
    inh <- S.openBinaryFile "u.cxm" ReadMode
    instr <- B.hGetContents inh
    hClose inh
    case parse parseCMX instr of
        Right c -> return (Just c)
        Left err -> do
            S.putStrLn err
            return (Nothing)