-- Pascals Triangle (from http://stackoverflow.com/questions/5188286/idiomatic-efficient-haskell-append)
-- The guy on stackoverflow was obsessed with the fact that he was append at the end of the string and that this is not efficient.
-- He wanted a better way. He liked the following

import qualified Data.Sequence as DS
import Data.Sequence ((|>), (<|))

-- If you import zipWith into the namespace then you need to hide the standard Prelude one
--import Prelude hiding (zipWith)

-- Again the initial first thoughts are WTF!!!!

-- OK so I can understand this. Need to use singleton to make it a Data.Sequence
ptri2 = DS.singleton 1 : mkptri2 ptri2

-- Well pretty obvious really once you look closely and realise that we are using overloaded |> and <| operators that are
-- appending to beginning or end of a sequence
mkptri2 (seq:seqs) = newRow : mkptri2 seqs
    where newRow = DS.zipWith (+) seq (0 <| seq) |> 1  -- NOTE: You can access the Data.Sequence operators via D.<| but this does not look nice



