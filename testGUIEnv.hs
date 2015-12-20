
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

--------------Test of hint----------
testOfHint = do
    varC <- varCreate initChessBoard
    let ioVar = initProgress varC
    hint ioVar


initProgress varVar = do
    cmxObj <- loadFromFile "u.cxm"
    case cmxObj of 
        Nothing -> varUpdate varVar (id)
        Just c-> do
            varUpdate varVar (\_ -> loadChessBoard c)

---------------Test of load-----------------
testOfLoad = do
    inh <- S.openBinaryFile "u.cxm" ReadMode
    instr <- B.hGetContents inh
    hClose inh
    case parse parseCMX instr of
        Right c -> return (Just c)
        Left err -> do
            S.putStrLn err
            return (Nothing)

--------------Test of export-----------------
testOfExport :: IO ()
testOfExport = do
    varC <- varCreate initChessBoard
    initProgress varC
    cb <- varGet varC
    --Here cb is a pure complete ChessBoard
    --if cb came from user data you can:
    --use varGet to convert Var ChessBoard to IO ChessBoard
    --use <- to take out ChessVBoard from IO
    --use expandChessBoard to rebuild the header
    --Then you can write it to file like following:
    ((exportToFile "new.cxm") . saveChessBoard) cb
