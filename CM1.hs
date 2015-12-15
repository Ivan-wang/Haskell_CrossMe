-----Fighting for your project!-----------
-------------Cross Me--------------------
module Main where

import Graphics.UI.WX
import Graphics.UI.WXCore


main :: IO()
main
 = run gui
 
gui :: IO()
gui
  =  do -- create top frame
       f <- frameCreateTopFrame "CrossMe"
       q <- button f [ text := "quit" , on command := close f ]
       h <- button f [ text := "help" , on command := chelp f ]
       a <- button f [ text := "about", on command := about f ]
       --s <- scrolledWindowCreate f idAny rectNull (wxHSCROLL + wxVSCROLL + wxNO_FULL_REPAINT_ON_RESIZE + wxCLIP_CHILDREN)
       p <- panel  f [ clientSize := sz 400 300 ]
       -- virtual size is 20*40 = 800 pixels
       --scrolledWindowSetScrollbars s 20 20 40 40 0 0 False
       
       set f [ layout := column 0
                         [ fill $ widget p
                         , hfloatCentre $ margin 5 $ row 5 [widget q, widget h, widget a]
                         ]
             , defaultButton := q
             ]
             
       vballs <- varCreate []
       windowOnPaint p (paintBalls vballs)

       -- left-click: new ball, right-click: new window
       windowOnMouse p False {- no motion events -} (onMouse1 p vballs)
       windowShow f
       windowRaise f
       return ()

onMouse1 w vballs mouse
      = case mouse of
          MouseLeftDown pt mods  -> dropBall w vballs pt  -- new ball
          --MouseRightDown pt mods -> ballsFrame            -- new window with bouncing balls
          other                  -> skipCurrentEvent      -- unprocessed event: send up the window chain

    -- add a new ball
dropBall w vballs pt
      = do varUpdate vballs ([ pt]:)
           windowRefresh w False

    -- paint the balls
paintBalls vballs dc  viewRect --updateAreas
      = withBrushStyle (BrushStyle BrushSolid white)  $ \brushWhite ->
        --withPenStyle (penColored blue 5) $ \penMedBlue ->
        do -- dcClearRect dc view
           dcSetBrush dc brushWhite
           dcDrawRectangle dc (rect (pt 20 20) (sz 500 500))
           drawgrid dc (1, (Rect 80 80 20 20))
           drawgrid2 dc ([1,0,-1,1,1] ,(Rect 100 100 20 20))
           drawgrid3 dc [[1,1,0,-1,-1],[0,1,-1,1,0],[0,0,-1,1,1]](Rect 120 120 20 20)
           drawLine2 dc (pt 80 80)10 5
           drawLine2_ dc (pt 80 80)10 5
           writeNum3 dc (pt 85 85) [[1,2,3,0,5],[2,1,2,3,1],[2,3,4,9,0,5]]
       
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
drawgrid dc (n, (Rect x y w h))
      |n==1      = dcWithBrushStyle dc (BrushStyle BrushSolid darkgrey) $
        dcDrawRectangle dc (rect (pt x y) (sz w h))
      |n== -1  = dcWithBrushStyle dc (BrushStyle (BrushHatch HatchCrossDiag) black) $
         dcDrawRectangle dc (rect (pt x y) (sz w h))
      |n==0  = return()
--drawgrid2::DC()->[Int]->Rect->IO()      
drawgrid2 dc (m, (Rect x y w h))=mapM_(drawgrid  dc)[(a, (Rect (x+20*b) y 20 20))|(a,b)<-zip m [0..(length(m)-1)]]
--drawgird3
drawgrid3 dc mm (Rect x y w h)=mapM_(drawgrid2 dc)[(a,(Rect x (y+20*b)20 20))|(a,b)<-zip mm [0..(length(mm)-1)]]

--------------------------------------------------------------------
about :: Window a -> IO ()
about w
  = infoDialog w "About Camels" "Camels\n\nby Maarten Löfler\nmloffler@cs.uu.nl\n\nCamels was written using wxHaskell"

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