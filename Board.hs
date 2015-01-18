module Board where
import System.Environment 

data Field = Tank Int Int | House Int Int | Empty Int Int

type DescX = [Int]

type DescY = [Int]

data Board = Board [((Int, Int), Field)] [Int] [Int]

toHouse :: (Int, Int) -> Field
toHouse x = (House (fst x) (snd x))

generateFields :: Int -> Int -> [((Int, Int), Field)]
generateFields rows columns = [((x,y), (Empty x y)) | x <- [0..rows], y <- [0..columns]]

toField :: (Int, Int) -> [(Int, Int)] -> ((Int, Int), Field)
toField coord houses = if coord `elem` houses 
	then (coord, (House (fst coord) (snd coord)))
	else (coord, (Empty (fst coord) (snd coord)))

parseBoard :: String -> String -> String -> Board
parseBoard line1 line2 line3 = 
	let rows = read line1 :: [Int]
	    columns = read line2 :: [Int]
	    inputHouses = read line3 :: [(Int, Int)]	
	    --fields = [ toHouse x | x <- inputHouses ]
	    parsedFields = [ toField (fst x) inputHouses | x <- generateFields (length rows) (length columns) ]
	in (Board parsedFields rows columns)

instance Show Field where
	show (Tank _ _) = " T "
	show (House _ _) = " H "
	show (Empty _ _) = "   "

instance Show Board where
	show (Board fields rows columns) = show fields	