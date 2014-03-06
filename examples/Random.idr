module Main

import Effect.Random
import Effect.Exception
import Effect.StdIO

data Err = NotANumber | OutOfRange

parseNumber : Int -> String -> { [EXCEPTION Err] } Eff m Int
parseNumber num str 
   = if all isDigit (unpack str) 
        then let x = cast str in
             if (x >=0 && x <= num) 
                then pure x
                else raise OutOfRange
        else raise NotANumber

guess : Int -> { [STDIO] } Eff IO ()
guess target 
    = do putStr "Guess: "
         case run (parseNumber 100 (trim !getStr)) of
              Nothing => do putStrLn "Invalid input"
                            guess target
              Just v => case compare v target of
                             LT => do putStrLn "Too low"
                                      guess target
                             EQ => putStrLn "You win!"
                             GT => do putStrLn "Too high"
                                      guess target

game : { [RND, STDIO] } Eff IO ()
game = do srand 123456789
          guess (fromInteger !(rndInt 0 100))

main : IO ()
main = run game

