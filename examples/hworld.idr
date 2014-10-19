module Main

import Effect.StdIO

hello : { [STDIO] } Eff ()
hello = putStrLn "Hello world!"

main : IO ()
main = run hello
