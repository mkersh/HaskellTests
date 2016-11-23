{- 
Showing how monad type structures can help with the uncertainty problem.

Uses the blackjack scoring problem to demonstrate uncertainty.

This is a simplistic deterministic version (where an Ace is always counted as a 1). In uncertain2.hs
we will see how a monad can help. NOTE: I probably shouldn't call them monads because they are not 
monads in the haskell sense of the definition BUT they use the same function composition idiom that
monads use.
-}

data Card =   Ace
            | Number Integer
            | Picture

valueCard :: Card -> Integer
valueCard Ace = 1 -- This is what we eventually want to make non-deterministic. i.e. The Ace can either be 1 or 11. For now though we keep it simple and just assume it is 1            
valueCard Picture = 10
valueCard (Number x) = x

valueHand::[Card] -> Integer
valueHand [] = 0
valueHand (c:cs) = (valueCard c) + (valueHand cs)

main = do 
    print (valueHand [])
    print (valueHand [Ace, Number 2, Number 3])
    print (valueHand [Ace, Picture])
    print (valueHand [Ace, Picture, Picture])

{-
So the above is a simple (but wrong) definition of how to value blackjack hands.
In uncertain2.hs we look on how to improve this.
-}

