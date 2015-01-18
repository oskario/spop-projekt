module Board where
import System.Environment 
import Data.List
import Data.Maybe
import Debug.Trace

data Field = Field { fieldType::FieldType, x::Int, y::Int }

data FieldType = Tank | House | Empty | Invalid | Marked deriving Eq

type DescX = [Int]

type DescY = [Int]

data Board = Board { fields::[Field], rows::[Int], cols::[Int] }

updateField :: Board -> Field -> Board
updateField input @ (Board fields rows cols) newField = 
	let newFields = [ updated | oldField <- fields, let updated = if ((x oldField) == (x newField) && (y oldField) == (y newField)) then newField else oldField ]
	in (Board newFields rows cols)

toHouse :: (Int, Int) -> Field
toHouse x = (Field House (fst x) (snd x))

generateFields :: Int -> Int -> [((Int, Int), Field)]
generateFields rows columns = [((x,y), (Field Empty x y)) | x <- [0..rows-1], y <- [0..columns-1]]

toField :: (Int, Int) -> [(Int, Int)] -> Field
toField coord houses = if coord `elem` houses 
	then (Field House (fst coord) (snd coord))
	else (Field Empty (fst coord) (snd coord))

parseBoard :: String -> String -> String -> Board
parseBoard line1 line2 line3 = 
	let rows = read line1 :: [Int]
	    columns = read line2 :: [Int]
	    inputHouses = read line3 :: [(Int, Int)]	
	    parsedFields = [ toField (fst x) inputHouses | x <- generateFields (length rows) (length columns) ]
	in (Board parsedFields rows columns)

placeTanks :: Board -> Board
placeTanks inputBoard = 
	let boardWithInvalidMarked = (markFieldsInvalid inputBoard)
	in boardWithInvalidMarked

markFieldsInvalid :: Board -> Board
markFieldsInvalid board @ (Board fields a b) = 
	let newFields = [ f | field <- fields, let nbours = neighbours isHouse (x field) (y field) board, let f = if (length nbours) > 0 then field else markInvalid field ]
	in updateField (Board newFields a b) (Field Tank 2 4)

markInvalid :: Field -> Field
markInvalid (Field Empty x y) = (Field Invalid x y)
markInvalid x @ (Field _ _ _) = x

markTank :: Field -> Field
markTank (Field Empty x y) = (Field Tank x y)
markTank x @ (Field _ _ _) = x

markMarked :: Field -> Field
markMarked (Field Empty x y) = (Field Marked x y)
markMarked x @ (Field _ _ _) = x

placeSingleNeighbouredTanks :: Board -> Board
placeSingleNeighbouredTanks board @ (Board fields rows cols) =
	--trace ("Placing on:\n" ++ show board)
	--trace ("Is correct: " ++ show (isBoardCorrect board))
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
	foldl (\a b -> a && b) True [ correct | field <- fields, let correct = isFieldCorrect field board ]

isFieldCorrect :: Field -> Board -> Bool
isFieldCorrect field board = let attachedTanks = neighbours isTank (x field) (y field) board
	in if fieldType field == House then length attachedTanks == 1 else True 

instance Show Field where
	show (Field Tank _ _) = " T "
	show (Field House _ _) = " H "
	show (Field Empty _ _) = "   "
	show (Field Marked _ _) = " M "
	show (Field Invalid _ _) = " X "

instance Show Board where
	show (Board fields rows columns) = 
		"  " ++ show [ show c | c <- columns ] ++ "\n" ++
		unlines [ show (rows !! index) ++ " " ++ show x | let listed = (splitEvery (length rows) fields), (x, index) <- listed `zip` [0..]]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list