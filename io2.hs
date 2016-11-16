import System.IO
import Data.Char(toUpper)

-- Just want to test to see if this behaves differently when run as an executable rather than under ghci
-- Under ghci the input typed gets echoed immediately and CTRL-D does not terminate the input stream
--
-- Yes when you run as an program "runghc io2.hs" it echos after each line and terminates the input when you type CTRL-D
main = do 
        -- This is quite a nice example as well of how you can combine functions together
        -- It contains two examples of functional currying
        -- It also converts the ++ operator into a prefix function (to make it more readable)
        interact ((++) "Your data, in uppercase, is:\n\n" . map toUpper)
        -- We could have done the following with the ++ 
        --interact (("Your data, in uppercase, is:\n\n" ++ ) . map toUpper)
