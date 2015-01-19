module Main where
import System.Environment 
import Board

-- loads input file and produces parsed Board
loadInputFile :: String -> IO Board
loadInputFile fileName = do
	putStrLn ("Opening " ++ fileName ++ "...")
	contents <- readFile fileName
	let linesArray = lines contents
	    rows = linesArray !! 0
	    columns = linesArray !! 1
	    houses = linesArray !! 2 in 
	    return $ parseBoard rows columns houses

-- reads file indicated by filename and solves the puzzle
solve :: String -> IO ()
solve fileName = do
	inputBoard <- loadInputFile fileName
	putStrLn ("Loaded board: \n" ++ show inputBoard)
	let solvedBoard = placeTanks inputBoard
	putStrLn ("Solution: \n" ++ show solvedBoard)	
	
-- prints program usage
printUsage :: String -> IO ()
printUsage progName = do
	putStrLn ("Usage: " ++ progName ++ " [input_file] [-save output_file]")

-- runs the program verifying provided arguments
run :: [String] -> String -> IO ()
run args progName
	| argCount == 1 = solve (args !! 0)
	| argCount == 3 && (args !! 1) == "-save" = solveWithSave (args !! 0) (args !! 2)
	| otherwise = printUsage progName
	where argCount = length args

solveWithSave :: String -> String -> IO ()
solveWithSave fileName outputFile = do
	inputBoard <- loadInputFile fileName
	putStrLn ("Loaded board: \n" ++ show inputBoard)
	let solvedBoard = placeTanks inputBoard
	writeFile outputFile ((showSolution solvedBoard) ++ "\n")
	putStrLn ("Solution: \n" ++ show solvedBoard)	

-- main function
main = do  
    args <- getArgs 
    progName <- getProgName 
    run args progName
