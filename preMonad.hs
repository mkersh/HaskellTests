{- 
http://book.realworldhaskell.org/read/code-case-study-parsing-a-binary-data-format.html has a whole chapter on how to
parse a binary data format that it claims is essential to understand the need for Monads.

This file are my experiments with the code in this chapter.

NOTE: I am not expecting this to be easy though. The book has many comments from people who found this chapter extremely difficult.
UPDATE: I got through this after experimenting with it for about 2 days. It was a bruising battle but I feel good about it now.
-}

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace,chr,isDigit)
import Data.Int (Int64)
import Data.Word (Word8)
import Control.Applicative ((<$>)) -- When importing operators you need to enclose in () <$> is a opertor version of Functor::fmap

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

-- The next section is me making changes from the example in the book
-- Just want to add another Parse stage to make sure I am beginning to understand what's going on here
mkState :: Parse ()
-- Just found out you can't use a where inside a lambda. So had to use let instead
mkState = Parse (\initState -> let nextState = initState { string = (L8.pack "HELLO")} in Right ((), nextState))
parseByte2 :: Parse Word8
parseByte2 = mkState ==> (\_ -> parseByte)


bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser  =  Parse chainedParser
  -- The thing that is confusing me is how the initState gets through into the chainedParser.
  -- I think I am over thinking this though.
  -- This ==> operator returns a higher order function (albeit wrapped in a type) and this takes a ParserState as a param
  -- 
  -- This way of chaining parsers together works left to right. i.e. firstParser works first and then passes results to secondParser
  -- Compare this to function composition using (.) This works from right to left
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState

testparse3 = parse parseByte2 (L8.pack "foo")

-- *************************************************************************************************************
-- So let's start to look at replacing our PGM parser using the new abstractions and some more to be introsuced


-- The next step on this journey is to take a look at Functors and how they will work with our parser.
-- Quick reminder on Functors before we start. They are a generic for of the map function (map being specific to lists).
-- They define a classtype that defines how to iteract through a data structure.

data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a)   = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

-- The following is the definition of Functor. 
-- BUT this comes as part of Prelude Library so I will comment out here
{- }
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-} 

-- Now we define Functor for our Tree data type
instance Functor Tree where
    fmap = treeMap

funcTest1 = fmap length ["foo","quux"]
funcTest2 = fmap length (Node (Leaf "Livingstone") (Leaf "I presume"))

-- We can think of fmap as a kind of lifting function. It takes a function over ordinary values a -> b and lifts it
-- to become a function over containers f a -> f b, where f is the container type

-- For here's our definition of fmap for Parse
instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)


funcTest3 = parse (id <$> parseByte) input
    where input = L8.pack "foo"

funcTest4 = parse ((chr . fromIntegral) <$> parseByte) (L8.pack "foo")
funcTest5 = parse (chr <$> fromIntegral <$> parseByte) (L8.pack "foo")

w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte

funcTest6 = parse parseChar (L8.pack "foo")

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

funcTest7 = parse peekByte (L8.pack "foo")

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte

funcTest8 = parse peekChar (L8.pack "foo")

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []

-- Nearly there. This is the new version of the parser that we are aiming for
-- It really does look quite alien when you come from an imperitive background.
-- We have sort of created a DSL here for parsing binary files.
-- The ==> ===>& and <$> all provide syntax sugar to allow you to express the chaining together of the Parse stages
--
-- What are my current thoughts? 
-- * It is beginning to grow on me. I've had moments when I just wanted to give up on this because it looked such a mess
-- but I'm glad that I haven't. This is the first time in a while with a programming language where I have really challenged myself 
-- to understand the code.
-- * It will be interesting when I revisit this code in the future whether it makes immediate sense next time.
--
-- I do feel however that understanding how code like this works is probably not for everyone.
parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")

-- This is a test for our parseRawPGM
parseTest3 = 
    do
        byteStr <- L.readFile "test.pgm"
        case parse parseRawPGM byteStr of
            Left err -> putStrLn "Problem Parsing the PGM bitmap file"
            Right bitmap -> putStrLn $ show bitmap

-- Just need to define the final missing helper functions
parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
-- The fmap and <$> below can be confusing (we have this in a number of places below)
-- Remember that <$> is just sugar for fmap
-- best way of thinking about fmap is that it allows you to apply a function to items in a container and return a new container with results of function
-- Our Parse container has Either types in it. To apply a function to the contents of the Either we need the second fmap
-- Take a look at parseChar and peekChar above to see the difference
-- 19th Nov 2016 - I am leaving this now still a little confused as to when we have to apply the double fmap
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        -- watch out for the record field accessor functions (string st) and offset st. These are extracting fields from the ParserState st
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h


-- ***********************************************************
-- Some more experiments with Functors and their fmap function
-- There was a lot of talk in chapter 10 RWH of using fmap to lift values out of a container to apply a function to them

mb1 = Just 34 :: Maybe Int
mb2 = Nothing :: Maybe Int
mb3 = Just 5 :: Maybe Int
ei1 = Left 1 :: Either Int String  -- You can't fmap over Either

-- So what fmap allows you to do is lift values out of a container (Maybe container in this case), apply a function to them
-- and then put back into the original container (possibly as a different type - not in this case though)
ft1 = (+2) <$> mb1  -- Using the abbreviated  operator syntax for fmap
ft2 = fmap (+2) mb2 -- Using the standard syntax

-- Having problems trying to create an alias for an fmap operation
-- You have to explicitly define the type (you dont't in ghci though)
-- and I can't get it to return something different from a. Whereas a functor can do??
ft3::(Num a, Num b, Functor f) => f a -> f a
ft3 = fmap (+2)


-- Learnt the case () trick from http://stackoverflow.com/questions/3416475/haskell-guards-on-lambda-functions
ft4::(Num a, Ord a, Functor f) => f a->f [Char]
ft4 con = fmap
  (\x -> case () of
    _ | x > 10 -> "Bigger than 10"
      | otherwise -> "less than 10"  ) con
