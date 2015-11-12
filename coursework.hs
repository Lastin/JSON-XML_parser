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

posForChar :: Integral a => Char -> String -> a -> a -> a -> a
posForChar _ [] _ _ _ = 0
posForChar c ('\\':ss) q e l = 1 + (posForChar c ss q (e + 1) l)
posForChar c (s:ss) q e l | even e && l == 0 && s == '"' = 1 + (posForChar c ss (q + 1) 0 l)
 | even q && l == 0 && s == c = 0
 | even q && s `elem` "{[" = 1 + (posForChar c ss q 0 (l + 1))
 | even q && s `elem` "]}" = 1 + (posForChar c ss q 0 (l - 1))
 | otherwise = 1 + (posForChar c ss q 0 l)

splitList :: Char -> String -> [String]
splitList c "" = []
splitList c s  = (take pos s:splitList c (drop (pos + 1) s))
               where pos = posForChar c s 0 0 0

-- TODO
escapeChars :: Char -> String
escapeChars c 
	| c == '\"' = "&quot"
	| c == '&' = "&amp"
	| c == '\'' = "&apos"
	| c == '<' = "&lt"
	| c == '>' = "&gt"
	| otherwise = [c]

prStr :: String -> String
prStr s = foldl (++) "" [ escapeChars char | char <- s]

indent l = replicate l '\t'
indent' l = "\n" ++ replicate l '\t'

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


translate :: String -> String
translate j = toXML $ fromJSON j

main = do
	[f, o, x] <- getArgs
	contents <- readFile f
	let objs = fromJSON contents
	writeFile o $ show objs
	writeFile x $ translate contents

{-
main = do
	[f, g] <- getArgs
	contents <- readFile f
	writeFile g $ translate contents-}
	

