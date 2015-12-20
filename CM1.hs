-----Fighting for your project!-----------
-------------Cross Me--------------------
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import ChessBoard
import Data.Matrix
import Status
import CXM
import Drawing
import System.IO as S
import Utils
import qualified Data.ByteString as B

main :: IO()
main = run gui
 
gui :: IO()
gui =  do 
    varA<-varCreate initname
    varC <- varCreate initChessBoard
    cb <- fileLoad varA
    case cb of
        Nothing -> varUpdate varC (id) 
        Just c -> do
            varUpdate varC (\old -> loadChessBoard c)
    f <- frameCreateTopFrame "CrossMe"
    q <- button f [ text := "quit" , on command := close f ]
    h <- button f [ text := "help" , on command := chelp f ]
    cre<-button f[text:="create",on command := createFrame]
    p <- panel  f [ clientSize := sz 400 300 ]
    a <- button f [ text := "hint", on command := hint' p  varC varA]
    
    set f [ layout := column 0
            [ fill $ minsize (sz maxX maxY) (widget p)
            , hfloatCentre $ margin 5 $ row 5 [widget q, widget h, widget a,widget cre]
            ]
        , defaultButton := q
        ]

    windowOnPaint p (paitGrids varC)
    windowOnMouse p False (onMouse p varC varA)
    windowShow f
    windowRaise f
    return ()

createFrame :: IO () 
createFrame = do 
                f <- frame [text := "Hello"]
                p <-panel f [ clientSize := sz 400 300 ]
                roww <- entry f [] 
                col <- entry f [] 
                name<-entry f[]
                varD <- varCreate initChessBoard
                varA<-varCreate (initname-1)
                saveit<- button f [text:="Save"
                                         ,on command:=  do 
                                                                                        cb<- varGet varD
                                                                                        let cbb= expandChessBoard cb
                                                                                        ((exportToFile "new.cxm").saveChessBoard)cbb
                                                                                        close f
                                         ]
                but <- button f [ text:="Show my grids"
                                , on command := do    getData varD roww col name p
                                ] 
                set f [ layout := column 0 
                         [ fill $ minsize (sz maxX maxY) (widget p)
                         , hfloatCentre $ margin 5 $ row 5 [label "row",widget roww, label"col",widget col,label"name", widget name]
                         , hfloatCentre $ margin 5 $ row 6[widget but, widget saveit]
                         ]]
                windowOnPaint p (paitGrids varD)
                windowOnMouse p False (onMouse p varD varA)               

onMouse w vgrids varA mouse
      = case mouse of
          MouseLeftDown pt mods  -> clickGrid w vgrids  varA pt 
          other   -> skipCurrentEvent     

clickGrid w varC varA pt
      = do
            c <- varGet varC 
            varUpdate varC ( switchLocation (rownum pt $ (nrows.hHeader)c)(colnum pt $(ncols.vHeader)c))
            checkend w varC varA
            windowRefresh w False

paitGrids varC dc  viewRect 
      = withBrushStyle (BrushStyle BrushSolid white )  $ \brushWhite ->
        do 
            dcSetBrush dc brushWhite
            dcDrawRectangle dc (rect (pt 0 0) (sz 500 600))
            user <- varGet varC
            let newY = 87 + 25 * ((nrows . hHeader) user)
                newX = 87 + 25 * ((ncols . vHeader) user)
                cols = ((ncols . hHeader) user) + ((ncols . vHeader) user)
                rows = ((nrows . vHeader) user) + ((nrows . hHeader) user)
            drawgrid3 dc ((toLists . userMosaic) user)(Rect (newX-7) (newY-7) 25 25)
            writeNum3 dc (pt newX 85) ((toLists . hHeader) user)
            drawLine2 dc (pt 80 80) cols rows
            drawLine2_ dc (pt 80 80) cols rows
            writeNum3 dc (pt 88 newY) ((toLists . vHeader) user)
       
checkend  pan varC varA= do
    newC <- varGet varC
    newA<- varGet varA
    if (newA/=(-1))
        then
            case checkMosaic newC of
                True -> do 
                                    repaint pan
                                    infoDialog pan "Level Up" ("Congratulations! It is "++name newC++"!")
                                    cb<-fileLoad varA
                                    case cb of
                                            Nothing -> varUpdate varC (id) 
                                            Just c -> do
                                                varUpdate varC (\old -> loadChessBoard c)
                                    repaint pan
                False -> repaint pan
    else repaint pan
--------------------------------------------------------------------
getData v r c  name p= 
    do
        let ioM = get r text
            ioN = get c text
            ioName = get name text
        m <- ioM
        n <- ioN
        na <- ioName
        varUpdate v (\old -> newChessBoard (str2Int m)  (str2Int n) na)
        repaint p
        
fileLoad  a = do
        name<-varGet a
        inh <- S.openBinaryFile (filenames!!name) ReadMode
        instr <- B.hGetContents inh
        hClose inh
        case parse parseCMX instr of
            Right c -> do
                varUpdate a (\_->nextname name)
                return (Just c)
            Left err -> do
                S.putStrLn err
                return (Nothing)
                
hint' p varC varA= do
    newC <- hint (varGet varC)
    varUpdate varC (\_->newC)
    repaint p
    checkend  p varC varA                
---------------------------------------------------------------------
maxX, maxY :: Int
maxY   = 400
maxX   = 325

filenames::[String]
filenames=["u.cxm","new.cxm","u.cxm"]
initname::Int
initname=0

nextname::Int->Int
nextname n 
            |n ==(length(filenames)-1)=0
            |otherwise = n+1

---------------------------------------------------------------------
chelp :: Window a -> IO ()
chelp w
  = infoDialog w "CrossMe Help"
  (  "How to play CrossMe\n\n"
  ++ "1.Click the block to change its status among blank, solid and shade\n"
  ++ "2.Numbers on the top and left imply the pattern of solid blocks.\n"
  ++ "3.A positive number implies the length of a sequence of solid blocks in this row or column.\n"
  ++ "4.Use one or more blank block to separate the solid block sequence to make them match the corresponding length.\n"
  ++ "5.Try to match the pattern by solid blocks and blank block. Shade blocks will help you in solving puzzle but only counts as blank block.\n"
  ++ "6. Match all pattern to get the whole picture.\n\n"
  ++ "Have fun!(●'◡'●)\n"
  )