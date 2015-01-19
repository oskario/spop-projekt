module Board where
import System.Environment 
import Data.List
import Data.Maybe
import Debug.Trace

data Field = Field { fieldType::FieldType, x::Int, y::Int, toX::Int, toY::Int }

data FieldType = House | Empty | Invalid | Marked | Tank deriving (Eq)

type DescX = [Int]

type DescY = [Int]

data Board = Board { fields::[Field], rows::[Int], cols::[Int] }

updateField :: Board -> Field -> Board
updateField input @ (Board fields rows cols) newField = 
	let newFields = [ updated | oldField <- fields, let updated = if ((x oldField) == (x newField) && (y oldField) == (y newField)) then newField else oldField ]
	in (Board newFields rows cols)

updateFields :: [Field] -> [Field] -> [Field]
updateFields oldFields (x:xs) = updateFieldInFields (updateFields oldFields xs) x 
updateFields oldFields [] = oldFields

updateFieldInFields :: [Field] -> Field -> [Field]
updateFieldInFields oldFields newField = [ updated | oldField <- oldFields, let updated = if ((x oldField) == (x newField) && (y oldField) == (y newField)) then newField else oldField ]

toHouse :: (Int, Int) -> Field
toHouse x = (Field House (fst x) (snd x) (-1) (-1))

generateFields :: Int -> Int -> [((Int, Int), Field)]
generateFields rows columns = [((x,y), (Field Empty x y (-1) (-1))) | x <- [0..rows-1], y <- [0..columns-1]]

toField :: (Int, Int) -> [(Int, Int)] -> Field
toField coord houses = if coord `elem` houses 
	then (Field House (fst coord) (snd coord) (-1) (-1))
	else (Field Empty (fst coord) (snd coord) (-1) (-1))

parseBoard :: String -> String -> String -> Board
parseBoard line1 line2 line3 = 
	let rows = read line1 :: [Int]
	    columns = read line2 :: [Int]
	    inputHouses = read line3 :: [(Int, Int)]	
	    parsedFields = [ toField (fst x) inputHouses | x <- generateFields (length rows) (length columns) ]
	in (Board parsedFields rows columns)

placeTanks :: Board -> Board
placeTanks inputBoard = 
	let solved = [board | tankVariation <- allPossibleTankVariations (markFieldsInvalid inputBoard), let board = withTankVariation inputBoard tankVariation, isBoardCorrect board] !! 0
	in solved

withTankVariation :: Board -> [Field] -> Board	
withTankVariation board @ (Board fields rows cols) tanks = 
	let newFields = updateFields fields tanks
	in (Board newFields rows cols)

markFieldsInvalid :: Board -> Board
markFieldsInvalid board @ (Board fields a b) = 
	let newFields = [ f | field <- fields, let nbours = neighbours isHouse (x field) (y field) board, let f = if (length nbours) > 0 then field else markInvalid field ]
	in (Board newFields a b)

markInvalid :: Field -> Field
markInvalid (Field Empty x y toX toY) = (Field Invalid x y toX toY)
markInvalid x @ (Field _ _ _ _ _) = x

markMarked :: Field -> Field
markMarked (Field Empty x y toX toY) = (Field Marked x y toX toY)
markMarked x @ (Field _ _ _ _ _) = x

placeSingleNeighbouredTanks :: Board -> Board
placeSingleNeighbouredTanks board @ (Board fields rows cols) =
	let newFields = [ f | field <- fields, let nbours = neighbours isHouse (x field) (y field) board, let f = if (length nbours) == 1 then markMarked field else field ]
	in Board newFields rows cols

neighbours :: (Field -> Bool) -> Int -> Int -> Board -> [Field]
neighbours f x y board @ (Board fields rows cols) = 
	maybeToList (getNeighbour f (x-1) (y) board) ++
	maybeToList (getNeighbour f (x+1) (y) board) ++
	maybeToList (getNeighbour f (x) (y-1) board) ++
	maybeToList (getNeighbour f (x) (y+1) board)

getNeighbour :: (Field -> Bool) -> Int -> Int -> Board -> Maybe Field
getNeighbour f a b (Board fields rows cols) = 
	find (\c -> x c == a && y c == b && f c) fields

isHouse :: (Field -> Bool)
isHouse = \field -> fieldType field == House

isEmpty :: (Field -> Bool)
isEmpty = \field -> fieldType field == Empty

isTank :: (Field -> Bool)
isTank = \field -> fieldType field == Tank

isBoardCorrect :: Board -> Bool
isBoardCorrect board @ (Board fields cols rows) =
	foldl (\a b -> a && b) True [ correct | c <- [0..(length cols)-1], let tanks = countTanks (getColumn board c), let correct = tanks == (rows !! c)] &&
	foldl (\a b -> a && b) True [ correct | r <- [0..(length rows)-1], let tanks = countTanks (getRow board r), let correct = tanks == (cols !! r)] &&
	foldl (\a b -> a && b) True [ correct | field <- fields, let correct = isFieldCorrect field board ]

isFieldCorrect :: Field -> Board -> Bool
isFieldCorrect field board = let attachedTanks = length [ place | place <- neighbours isTank (x field) (y field) board, (x field) == (toX place), (y field) == (toY place)]
	in 
	if fieldType field == House then 
		attachedTanks == 1 else 
		True 

getColumn :: Board -> Int -> [Field]
getColumn (Board fields rows cols) index = 
	[ row !! index | let xs = (splitEvery (length cols) fields), row <- xs]

getRow :: Board -> Int -> [Field]
getRow (Board fields rows cols) index = 
	(splitEvery (length cols) fields) !! index

countTanks :: [Field] -> Int
countTanks fields = length [ x | x <- fields, isTank x]

getPossibleTanksForField :: Field -> Board -> [Field]
getPossibleTanksForField (Field _ a b _ _) board @ (Board fields cols rows) =
	map (\f -> (Field Tank (x f) (y f) a b)) (neighbours isEmpty a b board)

getPossibleTanks :: Board -> [[Field]]
getPossibleTanks board @ (Board fields _ _) = [tanks | house <- fields, isHouse house, let tanks = getPossibleTanksForField house board]

allPossibleTankVariations :: Board -> [[Field]]
allPossibleTankVariations board = 
	sequence (getPossibleTanks board)

allVariations :: [[Int]] -> [[Int]]
allVariations input = sequence input

instance Show Field where
	show (Field Tank _ _ _ _) = " T "
	show (Field House _ _ _ _) = " H "
	show (Field Empty _ _ _ _) = "   "
	show (Field Marked _ _ _ _) = " M "
	show (Field Invalid _ _ _ _) = " X "

instance Show Board where
	show (Board fields rows columns) = 
		"  " ++ show [ show c | c <- columns ] ++ "\n" ++
		unlines [ show (rows !! index) ++ " " ++ show x | let listed = (splitEvery (length rows) fields), (x, index) <- listed `zip` [0..]]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list
