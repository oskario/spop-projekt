module Board where
import System.Environment 
import Data.List
import Data.Maybe
import Debug.Trace

-- main board structure
data Board = Board { fields::[Field], rows::[Int], cols::[Int] }

-- a single field on Board
data Field = Field { fieldType::FieldType, x::Int, y::Int, toX::Int, toY::Int }

-- all possible field types
data FieldType = House | Empty | Invalid | Tank deriving (Eq)

instance Show Field where
	show (Field Tank _ _ _ _) = " T "
	show (Field House _ _ _ _) = " H "
	show (Field Empty _ _ _ _) = "   "
	show (Field Invalid _ _ _ _) = " X "

instance Show Board where
	show (Board fields rows columns) =
		"  " ++ show [ show c | c <- columns ] ++ "\n" ++
		unlines [ show (rows !! index) ++ " " ++ show x | let listed = (splitEvery (length columns) fields), (x, index) <- listed `zip` [0..]]

-- places tanks on a Board
placeTanks :: Board -> Board
placeTanks inputBoard = 
	let solved = [board | tankVariation <- allPossibleTankVariations (markFieldsInvalid inputBoard), let board = withTankVariation inputBoard tankVariation, isBoardCorrect board] !! 0
	in solved

-- updates given list of fields with a new one (swaps fields with matching coordinates)
updateFields :: [Field] -> [Field] -> [Field]
updateFields oldFields (x:xs) = updateFieldInFields (updateFields oldFields xs) x 
updateFields oldFields [] = oldFields

-- updates given list of fields with a new field (swaps given field with a field that has matching coordinates)
updateFieldInFields :: [Field] -> Field -> [Field]
updateFieldInFields oldFields newField = [ updated | oldField <- oldFields, let updated = if ((x oldField) == (x newField) && (y oldField) == (y newField)) then newField else oldField ]

-- generates an empty list of Fields of a given size
generateFields :: Int -> Int -> [((Int, Int), Field)]
generateFields rows columns = [((x,y), (Field Empty x y (-1) (-1))) | x <- [0..rows-1], y <- [0..columns-1]]

-- converts coordinates to a proper Field structure
toField :: (Int, Int) -> [(Int, Int)] -> Field
toField coord houses = if coord `elem` houses 
	then (Field House (fst coord) (snd coord) (-1) (-1))
	else (Field Empty (fst coord) (snd coord) (-1) (-1))

-- parses given input file lines and produces a valid Board
parseBoard :: String -> String -> String -> Board
parseBoard line1 line2 line3 = 
	let rows = read line1 :: [Int]
	    columns = read line2 :: [Int]
	    inputHouses = read line3 :: [(Int, Int)]	
	    parsedFields = [ toField (fst x) inputHouses | x <- generateFields (length rows) (length columns) ]
	in (Board parsedFields rows columns)

-- applies given tank variation to a given board
withTankVariation :: Board -> [Field] -> Board	
withTankVariation board @ (Board fields rows cols) tanks = 
	let newFields = updateFields fields tanks
	in (Board newFields rows cols)

-- finds all fields that do not connect with any houses and marks them as invalid (there should be no Tanks not connected to any House)
markFieldsInvalid :: Board -> Board
markFieldsInvalid board @ (Board fields a b) = 
	let newFields = [ f | field <- fields, let nbours = neighbours isHouse (x field) (y field) board, let f = if (length nbours) > 0 then field else markInvalid field ]
	in (Board newFields a b)

-- marks given field as invalid one (there cannot be any Tank or House placed on it)
markInvalid :: Field -> Field
markInvalid (Field Empty x y toX toY) = (Field Invalid x y toX toY)
markInvalid x @ (Field _ _ _ _ _) = x

-- returns list of neighbours for a given field
neighbours :: (Field -> Bool) -> Int -> Int -> Board -> [Field]
neighbours f x y board @ (Board fields rows cols) = 
	maybeToList (getNeighbour f (x-1) (y) board) ++
	maybeToList (getNeighbour f (x+1) (y) board) ++
	maybeToList (getNeighbour f (x) (y-1) board) ++
	maybeToList (getNeighbour f (x) (y+1) board)

-- returns field indicated by a given position (a, b) as a Maybe
getNeighbour :: (Field -> Bool) -> Int -> Int -> Board -> Maybe Field
getNeighbour f a b (Board fields rows cols) = 
	find (\c -> x c == a && y c == b && f c) fields

-- checks if given board is correct, i.e. each house has exactly one tank associated with it
isBoardCorrect :: Board -> Bool
isBoardCorrect board @ (Board fields rows cols) =
	foldl (\a b -> a && b) True [ correct | c <- [0..(length cols)-1], let tanks = countTanks (getColumn board c), let correct = tanks == (cols !! c)] &&
	foldl (\a b -> a && b) True [ correct | r <- [0..(length rows)-1], let tanks = countTanks (getRow board r), let correct = tanks == (rows !! r)] &&
	foldl (\a b -> a && b) True [ correct | field <- fields, let correct = isFieldCorrect field board ]

-- checks if given field is correct, i.e. if it is a house it has to have one (and only one) tank connected
isFieldCorrect :: Field -> Board -> Bool
isFieldCorrect field board = let attachedTanks = length [ place | place <- neighbours isTank (x field) (y field) board, (x field) == (toX place), (y field) == (toY place)]
	in if fieldType field == House then attachedTanks == 1 else True 

-- get column of fields from a board indicated by a given index number
getColumn :: Board -> Int -> [Field]
getColumn (Board fields rows cols) index = 
	[ row !! index | let xs = (splitEvery (length cols) fields), row <- xs]

-- get row of fields from a board indicated by a given index number
getRow :: Board -> Int -> [Field]
getRow (Board fields rows cols) index = 
	(splitEvery (length cols) fields) !! index

-- count how many tanks are there in a given list of fields
countTanks :: [Field] -> Int
countTanks fields = length [ x | x <- fields, isTank x]

-- get all possible fields that tanks can be placed on (a list of fields is returned for each house)
getPossibleTanks :: Board -> [[Field]]
getPossibleTanks board @ (Board fields _ _) = [tanks | house <- fields, isHouse house, let tanks = getPossibleTanksForField house board]

-- get all possible neighbouring fields that a tank can be placed on
getPossibleTanksForField :: Field -> Board -> [Field]
getPossibleTanksForField (Field _ a b _ _) board @ (Board fields rows cols) =
	map (\f -> (Field Tank (x f) (y f) a b)) (neighbours isEmpty a b board)

-- returns all possible sets of fields, on which tanks can be placed
allPossibleTankVariations :: Board -> [[Field]]
allPossibleTankVariations board = 
	sequence (getPossibleTanks board)

-- returns all possible variations of a given lists of lists (taking one element from each list at a time)
allVariations :: [[Int]] -> [[Int]]
allVariations input = sequence input

-- returns true if given field is a type of House (for readability only)
isHouse :: (Field -> Bool)
isHouse = \field -> fieldType field == House

-- returns true if given field is a type of Empty (for readability only)
isEmpty :: (Field -> Bool)
isEmpty = \field -> fieldType field == Empty

-- returns true if given field is a type of Tank (for readability only)
isTank :: (Field -> Bool)
isTank = \field -> fieldType field == Tank

showSolution :: Board -> String
showSolution (Board fields rows cols) = show [(x field, y field) | field <- fields, isTank field]

-- splits given list in chunks with a maximum length of n
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list
