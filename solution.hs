import System.Environment 

data Board = Board [String] deriving (Show)

solve :: [Int] -> [Int] -> [(Int, Int)] -> [String] --[(Int, Int)]
solve rows columns homes = [show row | row <- rows]

loadInputFile :: String -> IO ()
loadInputFile fileName = do
	putStrLn ("Opening " ++ fileName ++ "...")
	contents <- readFile fileName
	let linesArray = lines contents
	    rows = read (linesArray !! 0) :: [Int]
	    columns = read (linesArray !! 1) :: [Int]
	    homes = read (linesArray !! 2) :: [(Int, Int)]
	    results = solve rows columns homes
	putStrLn ("Rows: " ++ show rows)
	putStrLn ("Columns: " ++ show columns)
	putStrLn ("Homes: " ++ show homes)
	putStrLn ("Result: " ++ show results)    
	

main = do  
    args <- getArgs 
    progName <- getProgName 
    if length args == 1
    	then loadInputFile (head args)
    	else putStrLn ("Usage: " ++ progName ++ " [input file]")
    return ()