module Board where
import System.Environment 
import Data.List

data Field = Tank Int Int | House Int Int | Empty Int Int | Invalid Int Int

--type Fields = [((Int, Int), Field)]

type DescX = [Int]

type DescY = [Int]

data Board = Board { fields::[Field], rows::[Int], cols::[Int] }

toHouse :: (Int, Int) -> Field
toHouse x = (House (fst x) (snd x))

generateFields :: Int -> Int -> [((Int, Int), Field)]
generateFields rows columns = [((x,y), (Empty x y)) | x <- [0..rows-1], y <- [0..columns-1]]

toField :: (Int, Int) -> [(Int, Int)] -> Field
toField coord houses = if coord `elem` houses 
	then (House (fst coord) (snd coord))
	else (Empty (fst coord) (snd coord))

parseBoard :: String -> String -> String -> Board
parseBoard line1 line2 line3 = 
	let rows = read line1 :: [Int]
	    columns = read line2 :: [Int]
	    inputHouses = read line3 :: [(Int, Int)]	
	    parsedFields = [ toField (fst x) inputHouses | x <- generateFields (length rows) (length columns) ]
	in (Board parsedFields rows columns)

placeTanks :: Board -> Board
placeTanks inputBoard = 
	let boardWithInvalidMarked = markFieldsInvalid inputBoard
	in boardWithInvalidMarked

markFieldsInvalid :: Board -> Board
markFieldsInvalid (Board fields a b) = 
	let newFields = [ markInvalid field | field <- fields ]
	in Board newFields a b

markInvalid :: Field -> Field
markInvalid x @ (Tank _ _) = x
markInvalid x @ (House _ _) = x
markInvalid (Empty x y) = (Invalid x y)
markInvalid x @ (Invalid _ _) = x

getFieldNeighbours :: Int -> Int -> Board -> [Field]
getFieldNeighbours x y (Board fields rows cols ) = fields

instance Show Field where
	show (Tank _ _) = " T "
	show (House _ _) = " H "
	show (Empty _ _) = "   "
	show (Invalid _ _) = " X "

instance Show Board where
	show (Board fields rows columns) = unlines [ show x | x <- (splitEvery (length rows) fields) ]

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list