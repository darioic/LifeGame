
module Life.Draw where
import Life.Board
import Drawing
import Data.Foldable (Foldable(foldMap))
import Life.Board (liveCells)

-- Dibuja en la casilla (x,y) un rectángulo 
drawCell :: Board -> Pos -> Drawing
drawCell board (x,y) = translated (fromIntegral x) (fromIntegral y) (solidRectangle 1 1) 


-- dibuja rectangulo(hace la funcion drawcell) en cada liveCell
-- liveCells contiene un array de (x,y) con todas las celdas activas
drawBoard :: Board -> Drawing
drawBoard board = foldMap (drawCell board) (liveCells board)





-- 'drawGrid minPos maxPos' obte el dibuix d'una graella que inclou els 2 extrems indicats.
-- 'minPos' es la posicio de l'extrem (esquerra, inferior) i
-- 'maxPos' es la posicio de l'extrem (dreta, superior)'.
-- NOTA: Aquesta funcio s'usa a partir del segon pas de la practica.
drawGrid :: Pos -> Pos -> Drawing
drawGrid minPos maxPos =
    colored gray $
        foldMap vline [fst minPos .. fst maxPos + 1]
        <> foldMap hline [snd minPos .. snd maxPos + 1]
    where
        hline row = polyline [(intToCoord (fst minPos), intToCoord row), (intToCoord (fst maxPos + 1), intToCoord row)]
        vline col = polyline [(intToCoord col, intToCoord (snd minPos)), (intToCoord col, intToCoord (snd maxPos + 1))]
        intToCoord i = fromIntegral i - 0.5


