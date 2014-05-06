{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding (Left, Right)
import qualified Data.Set as S
import Math.NumberTheory.Primes.Factorisation
import Data.Maybe
import Data.List
import Diagrams.Prelude
import Diagrams.TwoD.Text
import Diagrams.TwoD.Shapes
import Diagrams.Backend.SVG.CmdLine



type Board = [[Maybe Integer]]

showBoard = unlines . map ( unwords . map (maybe "-" show))

data Direction = Up | Down | Left | Right

main  = mainWith $ (renderBoard initialBoard :: Diagram B R2)

initialBoard = addOne
  [[Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing]
  ,[Nothing, Nothing, Nothing, Nothing, Nothing]
  ]

addOne :: Board -> Board
addOne [as, bs, cs, ds, Nothing:es] = [as, bs, cs, ds, Just 1:es]
addOne xs = xs

isLosing :: Board -> Bool
--sees whether all of the intersections on the board are occupied
--catMaybes throws away all of the nothings, then we see if the length of the resulting list is 4. Then, and together all of the booleans 
--to see whether all of the columns are full. If they are, you lose.
isLosing = and . map ((== 4) . length . catMaybes)

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

renderBox Nothing = square 2
renderBox (Just x) = square 2 `atop` text (show x)

renderBoard = vertically . map (horizontally . map renderBox)
