{- 
Refactoring of our original uncertain1.hs to add non-determinism into the solution.
This non-determinism is required because Ace can actually represent 1 or 11
-}

data Card =   Ace
            | Number Integer
            | Picture

valueCard :: Card -> P Integer
valueCard Ace = [1,11] -- This is what we eventually want to make non-deterministic. i.e. The Ace can either be 1 or 11. For now though we keep it simple and just assume it is 1            
valueCard Picture = [10]
valueCard (Number x) = [x]

valueHand::[Card] -> P Integer
valueHand [] = [0]
valueHand (c:cs) = (valueCard c) `addP` (valueHand cs)

main = do 
    print (valueHand [])
    print (valueHand [Ace, Number 2, Number 3])
    print (valueHand [Ace, Picture])
    print (valueHand [Ace, Picture, Picture])

type P a = [a]

composeP :: (b -> P c) -> (a -> P b) -> (a -> P c)
composeP f g x = [z | y <-g x, z <- f y] 

idP :: a -> P a
idP x = [x]

addP :: P Integer -> P Integer -> P Integer
addP xs ys = [x + y | x <- xs, y<-ys]

{-
See uncertain3.hs for a further refactoring
-}