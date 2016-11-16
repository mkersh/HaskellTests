{- First look at IO and monads in haskell -}

import System.IO
import Data.Char(toUpper)

io1 = do
       putStrLn "Greetings!  What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"

main :: IO ()
main = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       mainloop inh outh
       hClose inh
       hClose outh

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh
       if ineof
           then return ()
           else do inpStr <- hGetLine inh
                   hPutStrLn outh (map toUpper inpStr)
                   mainloop inh outh


-- Let's look at a simpler way of implementing main 
-- This uses hGetContents (h stands for handle) let binds all the content of the file to inputStr.
-- Because of lazy evaluation though the data in the file is only read as it is needed
io2 :: IO ()
io2 = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" WriteMode
       inpStr <- hGetContents inh
       let result = processData inpStr
       hPutStr outh result
       hClose inh
       hClose outh
       printFile "output.txt"
       

printFile :: String -> IO ()
printFile fpath = 
    do
        fileh <- openFile fpath ReadMode
        inpStr <- hGetContents fileh
        putStrLn $ inpStr

processData :: String -> String
processData = map toUpper

-- Another experiment to see if we can modify the content of the output file by seeking and changing
io3 :: IO ()
io3 = do 
       inh <- openFile "input.txt" ReadMode
       outh <- openFile "output.txt" ReadWriteMode
       inpStr <- hGetContents inh
       let result = processData inpStr
       hPutStr outh result
       printFile2 outh
       putStrLn $ "Now lets change file contents"
       -- Looks like the hGetContents is closing the handle passed in??
       -- Apparently the handle is set into a semi closed state (http://stackoverflow.com/questions/18160807/haskell-io-handle-closed)
       outh <- openFile "output.txt" ReadWriteMode
       writeTest2 outh
       printFile2 outh
       hClose inh
       hClose outh
       

printFile2 :: Handle -> IO ()
printFile2 fhandle = 
    do
        hSeek fhandle AbsoluteSeek 0
        inpStr <- hGetContents fhandle
        putStrLn $ inpStr

writeTest2 :: Handle -> IO ()
writeTest2 fhandle = 
    do
        hSeek fhandle AbsoluteSeek 2
        hPutStr fhandle "AAAAAAAAAAAAAAAAAAAAAAAAAA"


-- An even simpler way to read and write strings from/to files
io4 = do 
       inpStr <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inpStr)
       printFile "output.txt" 


-- interact gets a string from stdin
-- Get some funny results when running this in ghci
-- There's some suggestions that running
io5 = interact ((++) "Your data, in uppercase, is:\n\n" . 
                 map toUpper)



-- *****************************************
-- The following shows how you can store actions in pure code and apply them later
-- 
-- I'm not particularly impressed by this though. It is very difficult to see that str2action
-- will not necessarily print its results immediately.

str2action :: String -> IO ()
-- This does not get printed immediately because it was called from a pure function
str2action input = putStrLn ("Data: " ++ input)

-- So this is apparently a pure function, even though it calls an impure one
list2actions :: [String] -> [IO ()]
list2actions = map str2action

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
-- Original definition of actions was:
-- actions = list2actions strings
-- BUT to make it more obvious that the actions are not printed yet I have reversed the list
actions = (reverse . list2actions) strings


printitall :: IO ()
printitall = runall actions

-- Take a list of actions, and execute each of them in turn.
runall :: [IO ()] -> IO ()
runall [] = return ()
runall (firstelem:remainingelems) = 
    do firstelem
       runall remainingelems

io6 =
  -- Had problems with the indentation of do. Wasn't working for me at first. All the expressions have to be at the same level
  do 
    str2action "Start of the program"
    printitall
    str2action "Done!"


-- *****************************************
-- >> and >>= operators
-- These operators provide an alternative to do blocks to combine actions together
-- This is not a good enough reason though I'm sure there will be other advantages or reasons to use them over a do block

-- With >> the left action is executed for side effects and then the right action is run and results returned from this
io7 =  putStrLn "action one" >> putStrLn "action two"

-- with >>= th result of the first is passed to the second
io8 = getLine >>= putStrLn

-- You can think of them as a pipeline
io9 = putStrLn "action one" >> 
      putStrLn "action two" >>
      putStrLn "Now type some text and I will reflect it" >>
      getLine >>= 
      putStrLn


