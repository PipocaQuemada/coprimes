{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding (Left, Right)
import qualified Data.Set as S
import Math.NumberTheory.Primes.Factorisation
import Data.Maybe
import Data.List
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Diagrams.TwoD.Shapes
import Diagrams.Backend.Gtk
import Diagrams.Backend.Cairo (Cairo)
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.Events
import Control.Monad.Trans
import Data.IORef
import System.Random

type Board = [[Maybe Integer]]

showBoard = unlines . map ( unwords . map (maybe "-" show))

data Direction = Up | Down | Left | Right

main = do initGUI
          win <- windowNew
          onDestroy win mainQuit
          canvas <- drawingAreaNew
          board <- newIORef initialBoard
          gameOver <- newIORef False
          canvas `on` sizeRequest $ return (Requisition 512 512)
          set win [containerBorderWidth := 10, containerChild := canvas]
          canvas `on` exposeEvent $ liftIO (readIORef board 
                                   >>= defaultRender canvas . renderBoard)
                                   >> return True
          onKeyPress win $ \ (Key _ _ _ _ _ _ _ _ keyName _) -> 
            liftIO $ do b <- readIORef board
                        n <- randomRIO (1,5)
                        isGameOver <- readIORef gameOver
                        let b' = processEvent keyName n b
                        if isGameOver
                        then return ()
                        else
                          if isLosing b'
                            then defaultRender canvas (scale 20 $ text "You Lose!") 
                                 >> writeIORef gameOver True
                                 >> return ()
                            else if isWinning b'
                              then defaultRender canvas (scale 20 $ text "You Win!")
                                   >> writeIORef gameOver True
                                   >> return ()
                              else defaultRender canvas (renderBoard  b') 
                                   >>  writeIORef board b'
                                   >> return ()
                        return True
          widgetShowAll win
          mainGUI

processEvent "Up" n = addOne n . collapseDirection Up 
processEvent "Down" n = addOne n . collapseDirection Down  
processEvent "Left" n = addOne n . collapseDirection Left
processEvent "Right" n = addOne n . collapseDirection Right
processEvent _ _ = id


initialBoard = addOne 1
  [[Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing]
  ]

addOne :: Integer -> Board -> Board
addOne n [as, bs, cs, ds, Nothing:es] = [as, bs, cs, ds, Just n:es]
addOne n xs = xs

isLosing :: Board -> Bool
--sees whether all of the intersections on the board are occupied
--catMaybes throws away all of the nothings, then we see if the length of the resulting list is 4. Then, and together all of the booleans 
--to see whether all of the columns are full. If they are, you lose.
isLosing b = or (map (or . map (> 500) . catMaybes)  b) || and (map ((== 4) . length . catMaybes) b)

isWinning :: Board -> Bool
--see whether anything on the board is equal to one hundreds.
--Maps together all items on the board and checks to see if any of them are 100 (the winning condition). 
isWinning = or . fmap or . (fmap . fmap $ (== (Just 100)))  

collapse :: Board -> Board
collapse = map collapseLine

-- todo: test
collapseDirection ::  Direction -> Board -> Board
collapseDirection Up = transpose . collapse . transpose
collapseDirection Down = transpose . map reverse . collapse . map reverse . transpose 
collapseDirection Left = collapse
collapseDirection Right = map reverse . collapse . map reverse

collapseAdjacent :: Maybe Integer -> Maybe Integer -> Maybe Integer
collapseAdjacent (Just x) (Just y) = Just $ x + y
collaspeAdjacent _ _ = Nothing

collapsible :: Maybe Integer -> Maybe Integer -> Bool
collapsible (Just x) (Just y) = S.null $ S.intersection xfactors yfactors
  where xfactors = S.fromList . map fst $ factorise x 
        yfactors = S.fromList . map fst $ factorise y
collapsible _ _ = False

collapseLine :: [Maybe Integer] -> [Maybe Integer]
collapseLine [] = []
collapseLine [x] = [x]
collapseLine (Nothing : xs) = collapseLine (xs) ++ [Nothing]
--if you can collapse two things, collapse them. Otherwise, don't.
collapseLine (x : xs) = go $ collapseLine xs
  where  go [] = []
         go (y:ys) = if collapsible x y 
                          then collapseAdjacent x y : ys ++ [Nothing] 
                          else x : y : ys


---------------------------------------------------------------------
---- Display
---------------------------------------------------------------------

horizontally = foldl' (|||) mempty
vertically = foldl' (===) mempty

renderBox Nothing = square 10
renderBox (Just x) = square 10 `atop` (scale 5 $ text (' ':show x)) -- bug: rendering a single character fails on OSX

renderBoard = vertically . map (horizontally . map renderBox)

