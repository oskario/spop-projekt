module Board where
import System.Environment 

data Field = Tank Int Int | House Int Int | Empty Int Int

type DescX = [Int]

type DescY = [Int]

data Board = Board [Field] [Int] [Int]

toHouse :: (Int, Int) -> Field
toHouse x = (House (fst x) (snd x))

parseBoard :: String -> String -> String -> Board
parseBoard line1 line2 line3 = 
	let rows = read line1 :: [Int]
	    columns = read line2 :: [Int]
	    houses = read line3 :: [(Int, Int)]	
	    fields = [ toHouse x | x <- houses ]
	in (Board fields rows columns)

instance Show Field where
	show (Tank _ _) = " T "
	show (House _ _) = " H "
	show (Empty _ _) = "   "

instance Show Board where
	show (Board fields rows columns) = show fields	