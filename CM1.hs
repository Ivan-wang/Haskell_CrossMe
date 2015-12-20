-----Fighting for your project!-----------
-------------Cross Me--------------------
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore
import ChessBoard
import Data.Matrix
import Status
import CXM
import System.IO as S
import qualified Data.ByteString as B

main :: IO()
main = run gui
 
gui :: IO()
gui =  do -- create top frame
    varA<-varCreate initname
    varC <- varCreate initChessBoard
    cb <- testOfLoad varA
    case cb of
        Nothing -> varUpdate varC (id) 
        Just c -> do
            varUpdate varC (\old -> loadChessBoard c)
    f <- frameCreateTopFrame "CrossMe"
    q <- button f [ text := "quit" , on command := close f ]
    h <- button f [ text := "help" , on command := chelp f ]
    --s <- scrolledWindowCreate f idAny rectNull (wxHSCROLL + wxVSCROLL + wxNO_FULL_REPAINT_ON_RESIZE + wxCLIP_CHILDREN)
    p <- panel  f [ clientSize := sz 400 300 ]
    -- virtual size is 20*40 = 800 pixels
    --scrolledWindowSetScrollbars s 20 20 40 40 0 0 False
    a <- button f [ text := "hint", on command := hint' p  varC varA]
    
    set f [ layout := column 0
            [ fill $ minsize (sz maxX maxY) (widget p)
            , hfloatCentre $ margin 5 $ row 5 [widget q, widget h, widget a]
            ]
        , defaultButton := q
        ]

    windowOnPaint p (paintBalls varC)
    windowOnMouse p False {- no motion events -} (onMouse1 p varC varA)
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
                varA<-varCreate initname
                saveit<- button f [text:="save"
                                         ,on command:=  do 
                                                                                        cb<- varGet varD
                                                                                        let cbb= expandChessBoard cb
                                                                                        ((exportToFile "new.cxm").saveChessBoard)cbb
                                                                                        close f
                                         ]
                but <- button f [ bgcolor:=white,text:="ok"
                                , on command := do    getData varD roww col p
                                ] 
                set f [ layout := column 0 
                         [ fill $ minsize (sz maxX maxY) (widget p)
                         , hfloatCentre $ margin 5 $ row 5 [label "row",widget roww, label"col",widget col,label"name", widget name]
                         , hfloatCentre $ margin 5 $ row 6[widget but, widget saveit]
                         ]]
                windowOnPaint p (paintBalls varD)
                windowOnMouse p False {- no motion events -} (onMouse1 p varD varA)               

getData v r c  p= 
    do
        let ioM = get r text
            ioN = get c text
        m <- ioM
        n <- ioN
        varUpdate v (\old -> newChessBoard (str2Int m)  (str2Int n))
        repaint p

                        
str2Int :: String -> Int
str2Int  m  = read  m  :: Int
                        
onMouse1 w vballs varA mouse
      = case mouse of
          MouseLeftDown pt mods  -> dropBall w vballs  varA pt 
          MouseRightDown pt mods -> createFrame           
          other   -> skipCurrentEvent     

dropBall w varC varA pt
      = do
            c <- varGet varC 
            varUpdate varC ( switchLocation (rownum pt $ (nrows.hHeader)c)(colnum pt $(ncols.vHeader)c))
            checkend w varC varA
            windowRefresh w False

paintBalls varC dc  viewRect 
      = withBrushStyle (BrushStyle BrushSolid white)  $ \brushWhite ->
        do 
            dcSetBrush dc brushWhite
            dcDrawRectangle dc (rect (pt 0 0) (sz 500 600))
            user <- varGet varC
            let newY = 85 + 20 * ((nrows . hHeader) user)
                newX = 85 + 20 * ((ncols . vHeader) user)
                cols = ((ncols . hHeader) user) + ((ncols . vHeader) user)
                rows = ((nrows . vHeader) user) + ((nrows . hHeader) user)
            drawgrid3 dc ((toLists . userMosaic) user)(Rect (newX-5) (newY-5) 20 20)
            writeNum3 dc (pt newX 85) ((toLists . hHeader) user)
            drawLine2 dc (pt 80 80) cols rows
            drawLine2_ dc (pt 80 80) cols rows
            writeNum3 dc (pt 85 newY) ((toLists . vHeader) user)
       
--horizontal--         
drawLine::DC()->Int->Point->IO()
drawLine dc  n (Point x y)=
 do  dcWithPenStyle dc (penDefault{ _penWidth = 2, _penColor=rgb 0x70 0x80 0x90 }) $
             line dc (pt x y)(pt x (y+20*n))[]
drawLine2::DC()->Point->Int->Int->IO()
drawLine2 dc (Point a b) m n= mapM_ (drawLine dc n)[(pt (a+20*x) b)|x<-[0..m]]
--vertical--
drawLine_::DC()->Int->Point->IO()
drawLine_ dc  n (Point x y)=
 do  dcWithPenStyle dc (penDefault{ _penWidth = 2, _penColor=rgb 0x70 0x80 0x90 }) $
             line dc (pt x y)(pt (x+20*n) y)[]
drawLine2_::DC()->Point->Int->Int->IO()
drawLine2_ dc (Point a b)n m= mapM_ (drawLine_ dc n)[(pt 80(80+20*x))|x<-[0..m]]
--num--
writeNum:: DC()->Point->Int->IO()
writeNum  dc  (Point x y) n =
 do   if (n/=0) then dcWithFontStyle dc fontSwiss{ _fontSize = 10, _fontWeight = WeightBold } $
                                  do dcDrawText dc (show n)(pt x y)
                                  else return()
writeNum2::DC()->Point->[Int]->IO()
writeNum2 dc (Point x y) m = mapM_  (\a->do  writeNum dc (Point (x+20*a) y)(m!!a))[0..((length m)-1)]
writeNum3::DC()->Point->[[Int]]->IO()
writeNum3 dc (Point x y) mm = mapM_(\a->do writeNum2 dc(Point x (y+20*a))(mm!!a))[0..((length mm)-1)]
--drawgrid::DC()->Int->Rect->IO()
drawgrid dc (Set, (Rect x y w h)) 
                        = dcWithBrushStyle dc (BrushStyle BrushSolid darkgrey) $
                            dcDrawRectangle dc (rect (pt x y) (sz w h))
drawgrid dc (Unset,(Rect x y w h))
                        =dcWithBrushStyle dc (BrushStyle (BrushHatch HatchCrossDiag) black) $
                            dcDrawRectangle dc (rect (pt x y) (sz w h))
drawgrid dc (Unknown, (Rect x y w h))
                        =dcWithBrushStyle dc (BrushStyle BrushSolid white) $
                             dcDrawRectangle dc (rect (pt x y) (sz w h))
--drawgrid2::DC()->[Int]->Rect->IO()      
drawgrid2 dc (m, (Rect x y w h))=mapM_(drawgrid  dc)[(a, (Rect (x+20*b) y 20 20))|(a,b)<-zip m [0..(length(m)-1)]]
--drawgird3
drawgrid3 dc mm (Rect x y w h)=mapM_(drawgrid2 dc)[(a,(Rect x (y+20*b)20 20))|(a,b)<-zip mm [0..(length(mm)-1)]]

---------------------------------------------------------------------
newpt::Point->Point
newpt (Point x y) = (pt  (x-(x `mod` 20))(y-(y `mod` 20)))    
--------------------------------------------------------------------
rownum::Point->Int->Int
rownum (Point x y) toprow =((y-80)`div` 20 )-toprow+1

colnum::Point->Int->Int
colnum (Point x y) topcol = ((x-80)`div` 20)-topcol+1
--------------------------------------------------------------------
--checkend
checkend  pan varC varA= do
    newC <- varGet varC
    case checkMosaic newC of
        True -> do 
                                repaint pan
                                infoDialog pan "Level Up" (name newC)
                                cb<-testOfLoad varA
                                case cb of
                                            Nothing -> varUpdate varC (id) 
                                            Just c -> do
                                                varUpdate varC (\old -> loadChessBoard c)
                                repaint pan
        False -> repaint pan
--------------------------------------------------------------------
testOfLoad  a = do
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
---------------------------------------------------------------------
maxX, maxY :: Int
maxY   = 400
maxX   = 320
---------------------------------------------------------------------
filenames::[String]
filenames=["u.cxm","new.cxm","u.cxm"]
initname::Int
initname=0

nextname::Int->Int
nextname n = n+1
---------------------------------------------------------------------
hint' p varC varA= do
    newC <- hint (varGet varC)
    varUpdate varC (\_->newC)
    repaint p
    checkend  p varC varA

chelp :: Window a -> IO ()
chelp w
  = infoDialog w "Camels Help"
  (  "How to play Camels\n\n"
  ++ "The object of this puzzle is to move all the east looking camels to the eastern\n"
  ++ "end of the desert, and all the west looking camels to the west of the desert.\n"
  ++ "East looking camels can only move east, and west looking camels can only move\n"
  ++ "west. A camel can move one square forward (if that square is empty), or it can\n"
  ++ "jump over another camel if it is looking the OTHER way.\n\n"
  ++ "Once you succeed, you will advance to a higher level with more camels.\n\n"
  ++ "Good luck!\n"
  )