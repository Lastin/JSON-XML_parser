-- TODO: separate JSON documentation (see coursework description)
-- TODO: do part 3
-- TODO: comments

{-module Coursework
    (
        JValue(..),
        fromJSON,
        toXML,
        translate
    ) where-}

import System.Environment

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(JValue, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

{-
It converts elements from JSON string into objects type of JValue
first it drops space characters from the end of a string
then it drops the space characters from the head
Next it converts the string into appropriate JValue types
" - start of a string
0 to 9 is a number
t or f is boolean
[ is array
and { is objects
If neither of the guards matches, then JNull is returned

JArray stores the result of mapping every element returned from splitList with fromJSON
splitList splits the string at commas and returns it as array of strings
fromJSON is then called recursively

JObject stores a list of key-value pairs, which is created by splitting the input the same way as a list to obtain the pairs.
Then it is split again, but this time at the colon, to have the key and value separetely.
Those are then again parsed by calling fromJSON recursively.
-}
fromJSON :: String -> JValue
fromJSON (x:xs) | not (null xs) && isSpace (last xs) = fromJSON $ init (x:xs)
                | isSpace x                          = fromJSON xs
                | x == '"'                           = JString $ init xs
                | x `elem` ['0'..'9']                = JNumber (read (x:xs) :: Double)
                | x == 't'                           = JBool True
                | x == 'f'                           = JBool False
                | x == '['                           = JArray $ map fromJSON (splitList ',' (init xs))
                | x == '{'                           = let list = map (splitList ':') (splitList ',' (init xs))
                                                       in JObject [(fromJSON $ head item, fromJSON $ last item) | item <- list]
                | otherwise                          = JNull
                where isSpace = (`elem` " \t\n\r\f\v")

{-
Function posForChar returns the next position of a separator outside an enclosed string, array or an object
q - quotes
e - escapes
l - literals {[ or ]} 
-}
posForChar :: Integral a => Char -> String -> a -> a -> a -> a
posForChar _ [] _ _ _ = 0
posForChar c ('\\':ss) q e l = 1 + (posForChar c ss q (e + 1) l)
posForChar c (s:ss) q e l
	| even e && l == 0 && s == '"' = 1 + (posForChar c ss (q + 1) 0 l)
	| even q && l == 0 && s == c = 0
	| even q && s `elem` "{[" = 1 + (posForChar c ss q 0 (l + 1))
	| even q && s `elem` "]}" = 1 + (posForChar c ss q 0 (l - 1))
	| otherwise = 1 + (posForChar c ss q 0 l)

splitList :: Char -> String -> [String]
splitList c "" = []
splitList c s  = (take pos s:splitList c (drop (pos + 1) s))
               where pos = posForChar c s 0 0 0

{-
escapeChars function returns the escapes for most common special characters in the XML
If a character is not escaped, it is returned as a string
-}
escapeChars :: Char -> String
escapeChars c 
	| c == '\"' = "&quot"
	| c == '&' = "&amp"
	| c == '\'' = "&apos"
	| c == '<' = "&lt"
	| c == '>' = "&gt"
	| otherwise = [c]

{-
prStr is a helper function for printing strings. It iterates through every character
in the string, and calls escapeChars to escape special characters.
-}
prStr :: String -> String
prStr s = foldl (++) "" [ escapeChars char | char <- s]

{-
indent is a helper function which returns number of tabs
indent' returns same string as indent, but with new line character appended on head
-}
indent l = replicate l '\t'
indent' l = "\n" ++ replicate l '\t'

{-
toXML' prints JValue as a formated XML string
JObject is formated as <key>value<key> | (key, value) <- JObject
JArray is formated as 
	<array>
		<item>JValue</item>
		<item>JValue</item>
		...
		<item>JValue</item>
	</array>

toXML returs XML declaration followed by the result of toXML'
-}
toXML' :: JValue -> Int -> String
toXML' (JString s) l = prStr s
toXML' (JNumber n) l = show n
toXML' (JBool b) l 
	| b = "true"
	| otherwise = "false"
toXML' (JNull) l = "<null/>"
toXML' (JObject obj) l = indent l ++ foldl (++) "" [indent' l ++ "<" ++ (prStr key) ++ ">" ++ toXML' value (l+1) ++ "</" ++ (prStr key) ++ ">" |  (JString key, value) <- obj] ++ indent' (l-1)
toXML' (JArray a) l = indent' l ++ foldl (++) "<array>" [indent' (l+1) ++ "<item>" ++ toXML' i (l+1) ++ "</item>" | i <- a] ++ indent' l ++ "</array>" ++ indent' (l-1)
toXML :: JValue -> String
toXML x = "<?xml version=\"1.0\" encoding=\"utf-8\">" ++ toXML' x 0

{-
Function translate takes a JSON string and returns XML string
it passes the result of fromJSON to toXML
-}
translate :: String -> String
translate j = toXML $ fromJSON j


main = do
	[f, o, x] <- getArgs
	contents <- readFile f
	let objs = fromJSON contents
	writeFile o $ show objs
	writeFile x $ translate contents

