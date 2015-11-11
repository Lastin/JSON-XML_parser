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

posForChar :: Integral a => Char -> String -> a -> a -> a
posForChar _ []        _ _ = 0
posForChar c ('\\':ss) q e = 1 + (posForChar c ss q (e + 1))
posForChar c (s:ss)    q e | even e && s == '"' = 1 + (posForChar c ss (q + 1) 0)
                           | even q && s == c   = 0
                           | otherwise          = 1 + (posForChar c ss q 0)

splitList :: Char -> String -> [String]
splitList c "" = []
splitList c s  = (take pos s:splitList c (drop (pos + 1) s))
               where pos = posForChar c s 0 0

-- TODO
indent l = replicate l '\t'
toXML' :: JValue -> Int -> String
toXML' (JString s) l = s
toXML' (JNumber n) l = show n
toXML' (JBool b) l 
	| b = "true"
	| otherwise = "false"
toXML' (JNull) l = "<null/>"
toXML' (JObject obj) l = indent l ++ foldl (++) "" ["<" ++ key ++ ">" ++ toXML' value (l+1) ++ "</" ++ key ++ ">" |  (JString key, value) <- obj]
toXML' (JArray a) l = indent l ++ foldl (++) "<array>" ["<item>" ++ toXML' i (l+1) ++ "</item>" | i <- a] ++ "</array>"
toXML :: JValue -> String
toXML x = "<?xml version=\"1.0\" encoding=\"utf-8\">\n" ++ toXML' x 0


translate :: String -> String
translate j = toXML $ fromJSON j

main = do
	[f, g] <- getArgs
	contents <- readFile f
	writeFile g $ show $ fromJSON contents

{-
main = do
	[f, g] <- getArgs
	contents <- readFile f
	writeFile g $ translate contents-}
	

