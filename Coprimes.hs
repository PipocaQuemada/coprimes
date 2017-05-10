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

{- Coprimes is a 2048 clone, where instead of collapsing equal numbers we collapse coprime numbers by adding them together.
 - coprime means that the two numbers don't share any prime factors.  8 and 9 are coprime, but 6 and 9 aren't.
 - The object of the game is to create 100 befare you reach 500.
 - -}
         
main = do -- setup Gtk
          initGUI
          win <- windowNew
          onDestroy win mainQuit
          canvas <- drawingAreaNew
          canvas `on` sizeRequest $ return (Requisition 512 512)
          set win [containerBorderWidth := 10, containerChild := canvas]
          -- Gtk works in an event-based way.
          -- variables in Haskell are immutable
          -- so we need to use 'IORefs' to hold onto
          -- the game state.  IORefs are Haskell's equivalent of mutable variables.
          board <- newIORef initialBoard
          gameOver <- newIORef False
          -- display the board initially
          canvas `on` exposeEvent $ liftIO (readIORef board 
                                   >>= defaultRender canvas . renderBoard)
                                   >> return True
          -- the callback here is called every time any key is pressed
          onKeyPress win $ \ (Key _ _ _ _ _ _ _ _ keyName _) -> 
            liftIO $ do b <- readIORef board
                        -- get a random number between 1 and 5
                        -- do this here so we don't need to pass around the current seed
                        n <- randomRIO (1,5)
                        let b' = processEvent keyName n b
                        -- this callback will be called even after the game ends
                        -- so, check if the game is over and do nothing
                        isGameOver <- readIORef gameOver
                        if isGameOver
                        -- 'return ()', to a first approximation, means 'do nothing'
                        -- if statements in Haskell have to return the same type from every branch
                        -- so, have them all end with 'return ()' so they have uniform type
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
          -- rest of the Gtk setup
          widgetShowAll win
          mainGUI

---------------------------------------------------------------------
---- Model of the game
---------------------------------------------------------------------
type Board = [[Maybe Integer]]

-- useful for debugging in ghci
showBoard = unlines . map ( unwords . map (maybe "-" show))

data Direction = Up | Down | Left | Right

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

-- add a number to the lower left corner
addOne :: Integer -> Board -> Board
addOne n [as, bs, cs, ds, Nothing:es] = [as, bs, cs, ds, Just n:es]
addOne n xs = xs

isLosing :: Board -> Bool
--sees whether all of the intersections on the board are occupied
--catMaybes throws away all of the nothings, then we see if the length of the resulting list is 4. Then, and together all of the booleans 
--to see whether all of the columns are full. If they are, you lose.
-- You also lose if you go above 500
isLosing b = or (map (or . map (> 500) . catMaybes)  b) || and (map ((== 5) . length . catMaybes) b)

isWinning :: Board -> Bool
--see whether anything on the board is equal to one hundred.
--Maps together all items on the board and checks to see if any of them are 100 (the winning condition). 
isWinning = or . fmap or . (fmap . fmap $ (== (Just 100)))  

collapse :: Board -> Board
collapse = map collapseLine

-- collapsing is done from left to right
-- if we need to collapse in other directions, we rotate the board so that the direction we want
-- to collapse in goes from left to right, collapse, and then rotate the board back to normal.
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

-- lay out lists of diagrams either horizontally or vertically
horizontally :: [Diagram Cairo R2] -> Diagram Cairo R2
-- d1 ||| d2 lays out two diagrams horizontally
horizontally = foldl' (|||) mempty
vertically:: [Diagram Cairo R2] -> Diagram Cairo R2
-- d1 === d2 lays out two diagrams vertically
vertically = foldl' (===) mempty

renderBox :: Maybe Integer -> Diagram Cairo R2
renderBox Nothing = square 10
renderBox (Just x) = square 10 `atop` (scale 5 $ text (' ':show x)) -- bug: rendering a single character fails on OSX
                                                                    -- hacky solution: add a string so it doesn't fail

renderBoard :: Board -> Diagram Cairo R2
renderBoard = vertically . map (horizontally . map renderBox)
