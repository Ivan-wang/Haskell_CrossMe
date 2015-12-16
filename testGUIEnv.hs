import Graphics.UI.WX
import ChessBoard
import System.IO as S
import Data.ByteString as B
import Data.Matrix
import CXM
import Status
import Utils


main = do
    cb <- testOfLoad
    case cb of
        Nothing -> return (Nothing)
        Just c -> do
            varC <- varCreate c
            newC <- (varUpdate varC (switchLocation 1 1))
            return (Just newC)


testOfLoad = do
    inh <- S.openBinaryFile "u.cxm" ReadMode
    instr <- B.hGetContents inh
    hClose inh
    case parse parseCMX instr of
        Right c -> return (Just( loadChessBoard c))
        Left err -> do
            S.putStrLn err
            return (Nothing)