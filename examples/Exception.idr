module Main

import Effect.Exception
import Effect.StdIO
import Control.IOExcept

data Err = NotANumber | OutOfRange

instance Show Err where
    show NotANumber = "Not a number"
    show OutOfRange = "Out of range"

parseNumber : Int -> String -> { [EXCEPTION Err] } Eff m Int
parseNumber num str 
   = if all isDigit (unpack str) 
        then let x = cast str in
             if (x >=0 && x <= num) 
                then pure x
                else raise OutOfRange
        else raise NotANumber

readNum : Int -> { [STDIO] } Eff (IOExcept Err) ()
readNum r = do
    putStr $ "Enter a number between 0 and " ++ show r ++ ":"
    new {e = Exception Err} default
        (do num <- catch (parseNumber r (trim !getStr))
                 (\e => do putStrLn $ "FAIL: " ++ show e
                           return 0)
            putStrLn $ show num)

readNum' : Int -> { [STDIO, EXCEPTION Err] } Eff (IOExcept Err) ()
readNum' r = do
    putStr $ "Enter a number between 0 and " ++ show r ++ ":"
    num <- catch (parseNumber r (trim !getStr))
                 (\e => do putStrLn $ "FAIL: " ++ show e
                           return 0)
    putStrLn $ show num

main : IO ()
main = ioe_run (run (readNum 100)) (\e => return ()) (\ok => return ())

