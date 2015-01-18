module Main where
import System.Environment 
import Board

loadInputFile :: String -> IO Board
loadInputFile fileName = do
	putStrLn ("Opening " ++ fileName ++ "...")
	contents <- readFile fileName
	let linesArray = lines contents
	    rows = linesArray !! 0
	    columns = linesArray !! 1
	    houses = linesArray !! 2 in 
	    return $ parseBoard rows columns houses

solve :: String -> IO ()
solve fileName = do
	inputBoard <- loadInputFile fileName
	putStrLn ("Loaded board: \n" ++ show inputBoard)
	let solvedBoard = placeTanks inputBoard
	putStrLn ("Is correct: " ++ show (isBoardCorrect solvedBoard))
	putStrLn ("Solution: \n" ++ show solvedBoard)
	
printUsage :: String -> IO ()
printUsage progName = do
	putStrLn ("Usage: " ++ progName ++ " [input file]")

run :: [String] -> String -> IO ()
run args progName
	| argCount < 1 || argCount > 1 = printUsage progName
	| argCount == 1 = solve (args !! 0)
	where argCount = length args

main = do  
    args <- getArgs 
    progName <- getProgName 
    run args progName
