module Main

import Effect.StdIO

hello : { [STDIO] } Eff IO ()
hello = putStrLn "Hello world!"

main : IO ()
main = run hello
