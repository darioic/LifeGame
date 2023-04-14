
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Life.Board
import Life.Draw

import Drawing
import Drawing.Vector
import System.Process.Internals (translate)
import qualified Data.Text as T
-----------------------------------------------------
-- The game state

data Game = Game
        { gmBoard :: Board      -- last board generation
        , gmGridMode :: GridMode
        , gmZoom :: Double, gmShift :: Point
        , gmPaused :: Bool
        , gmInterval :: Time    -- generation interval when not paused
        , gmElapsedTime :: Time -- elapsed time from last generation
        }
    deriving (Show, Read)

setGmBoard x g       = g{ gmBoard = x }
setGmGridMode x g    = g{ gmGridMode = x }
setGmZoom x g        = g{ gmZoom = x }
setGmShift x g       = g{ gmShift = x }
setGmPaused x g      = g{ gmPaused = x }
setGmInterval x g    = g{ gmInterval = x }
setGmElapsedTime x g = g{ gmElapsedTime = x }

data GridMode = NoGrid | LivesGrid | ViewGrid
    deriving (Show, Read)

-----------------------------------------------------
-- Initialization

viewWidth, viewHeight :: Double
viewWidth = 60.0
viewHeight = 30.0

main :: IO ()
main =
    activityOf viewWidth viewHeight initial handleEvent draw

board0Cells =
    [(-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]

initial = Game
    { gmBoard = foldr (setCell True) initBoard board0Cells
    , gmGridMode = NoGrid
    , gmZoom = 1.0, gmShift = (0.0, 0.0)
    , gmPaused = True
    , gmInterval = 1.0 -- in seconds
    , gmElapsedTime = 0.0
    }

-----------------------------------------------------
-- A completar per l'estudiant
newGrid :: GridMode -> GridMode
newGrid NoGrid = LivesGrid
newGrid LivesGrid = ViewGrid
newGrid ViewGrid = NoGrid

-- Update shift based on zoom level

handleEvent :: Event -> Game -> Game


handleEvent (KeyDown " ") game =               
    if gmPaused game then setGmPaused False game 
    else setGmPaused True game
    
-- Limitamos min interval time a 0.125s
handleEvent (KeyDown "+") game =                
    if gmInterval game > 0.250 then setGmInterval (gmInterval game * 0.5) game
    else game

handleEvent (KeyDown "-") game =                
     setGmInterval(gmInterval game*2) game

-- si paused = False y gmElapsedTime+dt >= gmInterval pasamos a la siguiente generacion
handleEvent (TimePassing dt) game =                
    let (t, g) | not(gmPaused game) && gmElapsedTime game + dt >= gmInterval game = (0, setGmBoard(nextGeneration(gmBoard game))game)
               | otherwise = (gmElapsedTime game + dt, game)
    in setGmElapsedTime t g

-- Max zoom = 4
-- Min zoom = 0,5
handleEvent (KeyDown "I") game =                -- Zoom in
    if gmZoom game < 4.0 then setGmZoom (gmZoom game * 2.0) game
    else game

handleEvent (KeyDown "O") game =                -- Zoom out
    if gmZoom game > 0.5 then setGmZoom (gmZoom game * 0.5) game
    else game

-- 1/zoom para obtener valores (x,y) sin escalado
-- restar (0,1) mueve la vista 1 cuadricula hacia arriba
handleEvent (KeyDown "ARROWUP") game =               
    setGmShift (gmShift game ^+^ (1.0/gmZoom game) *^ (0,1)) game
      
handleEvent (KeyDown "ARROWDOWN") game =               
    setGmShift (gmShift game ^-^ (1.0/gmZoom game) *^ (0,1)) game

handleEvent (KeyDown "ARROWRIGHT") game =               
    setGmShift (gmShift game ^+^ (1.0/gmZoom game) *^ (1,0)) game

handleEvent (KeyDown "ARROWLEFT") game =               
    setGmShift (gmShift game ^-^ (1.0/gmZoom game) *^ (1,0)) game

handleEvent (KeyDown "G") game =                -- Next grid
    setGmGridMode (newGrid (gmGridMode game)) game

handleEvent (KeyDown "N") game =                -- Next generation
    setGmBoard (nextGeneration (gmBoard game)) game -- nextGen(gmboard game) nos da la nueva board(estado), y setGmBoard actualiza el field gmboard de game con este nuevo estado

handleEvent (MouseDown (x, y)) game =           -- Set live/dead cells
    let pos = (round x, round y)
        brd = gmBoard game
    in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game

handleEvent _ game =                            -- Ignore other events
    game

-----------------------------------------------------
-- Drawing

legend :: [(String, String)]
legend = [("SPACE", "Pause/Run mode"),
            ("+", "Increase speed"),
            ("-", "Decrease speed"),
            ("I", "Zoom in"),
            ("O", "Zoom out"),
            ("ARROW UP", "Move up"),
            ("ARROW DOWN", "Move down"),
            ("ARROW RIGHT", "Move right"),
            ("ARROW LEFT", "Move left"),
            ("G", "Change grid mode"),
            ("N", "Next generation")]

-- Dibuja una linea de la leyenda
draw1Legend :: (Int, (String, String)) -> Drawing
draw1Legend (index, (text1, text2)) =
    let x1 = (-viewWidth*0.5+48)
        x2 = x1 + 5
        y = viewHeight * 0.5 - 1 - fromIntegral index
        text1' = T.pack text1   --para pasar de string a text ya que me daba problemas en la funcion atext
        text2' = T.pack text2
    in colored blue (translated x1 y (atext startAnchor text1') <> translated x2 y (atext startAnchor text2'))

-- Dibuja toda la leyenda
-- zip asigna un indice[0...length] a cada una de las leyendas para dibujar en el eje y correspondiente

drawAllLegend leg = foldMap draw1Legend (zip[0..] leg)

draw game =
    drawAllLegend legend <>
    translated (fst(gmShift game)) (snd(gmShift game)) 
    (scaled (gmZoom game) (gmZoom game)
    (drawBoard (gmBoard game)) <>
    case (gmGridMode game) of
        NoGrid -> blank
        LivesGrid -> drawGrid (minLiveCell(gmBoard game)) (maxLiveCell (gmBoard game))
        ViewGrid -> drawGrid (-round viewWidth , -round viewHeight) (round viewWidth, round viewHeight) 

    )
