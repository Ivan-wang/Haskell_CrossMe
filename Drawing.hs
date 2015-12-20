module Drawing(
    drawLine2,
    drawLine2_,
    writeNum3,
    drawgrid3,
    newpt,
    rownum,
    colnum)where

import Graphics.UI.WX
import Graphics.UI.WXCore
import Status    
    
----horizontal lines----         
drawLine::DC()->Int->Point->IO()
drawLine dc  n (Point x y)=
 do  dcWithPenStyle dc (penDefault{ _penWidth = 3, _penColor=rgb 0x70 0x80 0x90 }) $
             line dc (pt x y)(pt x (y+25*n))[]
drawLine2::DC()->Point->Int->Int->IO()
drawLine2 dc (Point a b) m n= mapM_ (drawLine dc n)[(pt (a+25*x) b)|x<-[0..m]]

----vertical lines----
drawLine_::DC()->Int->Point->IO()
drawLine_ dc  n (Point x y)=
 do  dcWithPenStyle dc (penDefault{ _penWidth = 2, _penColor=rgb 0x70 0x80 0x90 }) $
             line dc (pt x y)(pt (x+25*n) y)[]
drawLine2_::DC()->Point->Int->Int->IO()
drawLine2_ dc (Point a b)n m= mapM_ (drawLine_ dc n)[(pt 80(80+25*x))|x<-[0..m]]

----write numbers----
writeNum:: DC()->Point->Int->IO()
writeNum  dc  (Point x y) n =
 do   if (n/=0) then dcWithFontStyle dc fontSwiss{ _fontSize = 11, _fontWeight = WeightBold} $
                                                 dcDrawText dc (show n)(pt x y)
                                     else return()
writeNum2::DC()->Point->[Int]->IO()
writeNum2 dc (Point x y) m =
             mapM_  (\a-> writeNum dc (Point (x+25*a) y)(m!!a))[0..((length m)-1)]
writeNum3::DC()->Point->[[Int]]->IO()
writeNum3 dc (Point x y) mm = 
             mapM_(\a->writeNum2 dc(Point x (y+25*a))(mm!!a))[0..((length mm)-1)]

----draw grids----
drawgrid::DC()->(Status,Rect2D Int)->IO()
drawgrid dc (Set, (Rect x y w h))= 
             dcWithBrushStyle dc (BrushStyle BrushSolid  darkgrey) $
             dcDrawRectangle dc (rect (pt x y) (sz w h))
drawgrid dc (Unset,(Rect x y w h))=
             dcWithBrushStyle dc (BrushStyle (BrushHatch HatchCrossDiag) black) $
             dcDrawRectangle dc (rect (pt x y) (sz w h))
drawgrid dc (Unknown, (Rect x y w h))=
             dcWithBrushStyle dc (BrushStyle BrushSolid white) $
             dcDrawRectangle dc (rect (pt x y) (sz w h))

drawgrid2::DC()->([Status],Rect2D Int)->IO()     
drawgrid2 dc (m, (Rect x y w h))=
             mapM_(drawgrid  dc)[(a, (Rect (x+25*b) y 25 25))|(a,b)<-zip m [0..(length(m)-1)]]

drawgrid3::DC()->[[Status]]->Rect2D Int->IO()     
drawgrid3 dc mm (Rect x y w h)=
             mapM_(drawgrid2 dc)[(a,(Rect x (y+25*b)25 25))|(a,b)<-zip mm [0..(length(mm)-1)]]

-----location switch----
newpt::Point->Point
newpt (Point x y) = (pt  (x-(x `mod` 25))(y-(y `mod` 25)))    

rownum::Point->Int->Int
rownum (Point x y) toprow =((y-80)`div` 25 )-toprow+1

colnum::Point->Int->Int
colnum (Point x y) topcol = ((x-80)`div` 25)-topcol+1