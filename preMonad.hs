{- 
http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html has a whole chapter on how to
parse a binary data format that it claims is essential to understand the need for Monads.

This file are my experiments with the code in this chapter.

NOTE: I am not expecting this to be easy though. The book has many comments from people who found this chapter extremely difficult.
-}

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Word (Word8)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

-- Normally we would derive Show to beable to print a Grepmap but because of the ByteString this would produce a very messy output
-- So instead we provide a custom version of Show
instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ 
                             " " ++ show m

-- **********************************************************************
-- Here's the first attempt at parsing the PGM file
--

parseTest1:: IO ()                              
parseTest1 = 
    do
        byteStr <- L.readFile "test.pgm"
        case parseP5 byteStr of
            Nothing -> putStrLn "Problem Parsing the PGM bitmap file"
            Just (bitmap, leftOver) -> putStrLn $ show bitmap

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmap, s6) ->
                              Just (Greymap width height maxGrey bitmap, s6)

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
                 | num <= 0    -> Nothing
                 -- It does not appear that you need the fromIntegral. Works fine in ghci with Just(num,rest)
                 | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both


-- *******************************************************************************
-- Let's try and improve on the above parseP5 - that's what the book says not me :)

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

parseTest2 = 
    do
        byteStr <- L.readFile "test.pgm"
        case parseP5_take2 byteStr of
            Nothing -> putStrLn "Problem Parsing the PGM bitmap file"
            Just (bitmap, leftOver) -> putStrLn $ show bitmap

parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
    matchHeader (L8.pack "P5") s      >>?
    -- This next line looks confusing because it is different from the subsequent skipspace lines below
    -- Reason for this difference is the matchHeader is passing over a Maybe L.ByteString whereas the rest are passing Maybe (a, L.ByteString)
    --      where the a's contain the parsed it details (i.e. height, width, maxGrey, bitmap)
    \s -> skipSpace ((), s)           >>?
    -- I don't think the skipSpace is needed above?? matchHeader already does this
    -- What's snd? Its a standard tuple function to return second element of tuple
    (getNat . snd)                    >>?
    -- Next skipSpace is called differently from above?? That's because getNat returns in format we require i.e. Maybe (a, L.ByteString)
    -- skipSpace is very badly named. I think this is the cause of a lot of confusion (as reported in the books comments)
    --      It is not just skipping space. It is also passing through (with no modification) the previous value i.e. first element of the tuple.
    skipSpace                         >>?
    \(width, s) ->   getNat s         >>?
    skipSpace                         >>?
    \(height, s) ->  getNat s         >>?
    \(maxGrey, s) -> getBytes 1 s     >>?
    (getBytes (width * height) . snd) >>?
    \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)

-- So is parseP5_take2 an improvement over parseP5?
-- It is certainly shorter with less code but IMO not an improvement.
-- It is a bit of a mess really. Took me quite a while to figure it out
-- BUT I suspect (and hope) that this is only part of the story and that eventually when I fully uderstand monads I will have one of those eureka moments.

-- *******************************************************************************
-- So the above was the first attempt to refactor the parsing function BUT the authors of the book
-- are still not happy:
-- The above is passing pairs around. 
--          With first part of pair containing previous parsed value and the second part current residual ByteString (i.e. bit we haven't processed yet)
--          They suggest that we may want to extend in furture and this rigid tuple design makes it difficult to change.
--              For example: to track the number of bytes we've consumed so that we can report the location of a parse failure 
-- So let's see how they suggest improving this:

-- Data type for storing ParserState
-- NOTE: It is difficult from just looking at the field names exactly what we will store in here??
-- Will the string be the residual string or the original complete string?? I suspect the residual string
-- The offset is presumably to cover the point they mentioned above about having a position of where we are upto in the original string. 
data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64           -- imported from Data.Int
    } deriving (Show)

-- The Big advantage with the new ParserState is that the record syntax means we can avoid using pattern matching
-- and pattern matching above in our first two attenpts was a problem because it made the code very brittle and difficult to change if we 
-- want to change/extend the state.

-- So definitions of Parsers based on our new ParseState would look something like
simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

-- Or better - allow for an error string to be returned as well of the ParseState
betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

-- Hide the implementation of our parser from our users
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }

-- Let's test out with a simple identity parser
identity :: a -> Parse a
-- The below confused me at first. The lambda function passed in bound to runParse field of Parse data type
identity a = Parse (\s -> Right (a, s))


parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
    -- This next line is/was confusing me. It would appear that runParse parser  is just returning the runParse field from the parser::Parse type
    -- IMO slightly less confusing if you put brackets around it (runParse parser) 
    = case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result

testparse1 = parse (identity 1) undefined
testparse2 = parse (identity "Hello World") undefined


-- Record syntax is useful not just for accessor functions (as seen above with runParser) but also to copy and partially change an existing value
modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset =
    initState { offset = newOffset }


-- OK. So now let's create a slightly more interesting parser. One that parses a single byte
-- All I can say really after staring at this for about 30 mins is WTF!!!
-- I have got it working under ghci BUT what the fuck is it actually doing
parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser  =  Parse chainedParser
  -- The thing that is confusing me is how the initState gets through into the chainedParser.
  -- I think I am over thinking this though.
  -- This ==> operator returns a higher order function (albeit wrapped in a type) and this takes a ParserState as a param
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState

testparse3 = parse parseByte (L8.pack "foo")

