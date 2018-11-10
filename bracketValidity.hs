-- Determines whether a String of brackets is a valid
-- PRE: s contains only bracket Chars
isValid :: String -> Bool
isValid s
  = isValid' s []

-- Checks if c is an opening bracket
isOpener :: Char -> Bool
isOpener c
  = elem c "({["

-- Checks if given Chars are of the same type of bracket
-- PRE: c1, c2 are bracket chars
isMatch :: Char -> Char -> Bool
isMatch '[' ']' 
  = True
isMatch '(' ')' 
  = True
isMatch '{' '}' 
  = True
isMatch ']' '[' 
  = True
isMatch ')' '(' 
  = True
isMatch '}' '{' 
  = True
isMatch c1 c2
  = c1 == c2

isValid' :: String -> [Char] -> Bool
isValid' "" []
  = True
isValid' "" (_ : _)
  = False
isValid' (b : bs) []
  = isOpener b && isValid' bs [b]
isValid' (b : bs) stack @ (c : cs)
  | isOpener b = isValid' bs (b : stack)
  | otherwise  = (isMatch b c) && isValid' bs cs 
