module Chatterbot where
import Utilities
import System.Random
import Data.Char
import Data.Maybe

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
stateOfMind _ = return id

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply _ = id

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = id

reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile _ = []


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard t s = concat [ replaceList x wildcard s| x <- t]
--   where
--     f x | x == wildcard = s 
--         | otherwise     = x

replaceList :: Eq a => a -> a -> [a] -> [a]
replaceList b c d 
  | b == c    = d
  | otherwise = [b]

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match wild pat sent = match2 wild pat sent pat sent
{- TO BE WRITTEN -}

match2 :: Eq a => a -> [a] -> [a] -> [a] ->[a]-> Maybe [a]
match2 wild pat sent oripat orisent
  | pat == [] && sent == [] = (Just (extractJust wild oripat orisent []))
  | pat == [] = Nothing
  | sent == [] = Nothing
  | (pat !! 0 /= wild) && ((pat !! 0) /= (sent !! 0)) = Nothing
  | (pat !! 0 /= wild) && ((pat !! 0) == (sent !! 0)) = match2 wild (tail pat) (tail sent) oripat orisent
  | isJust (singleWildcardMatch pat sent wild oripat orisent ) = singleWildcardMatch pat sent wild oripat orisent
  | isJust(longerWildcardMatch pat sent wild oripat orisent) = longerWildcardMatch pat sent wild oripat orisent
  | otherwise = Nothing
{- TO BE WRITTEN -}


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> a -> [a] -> [a]-> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) wild oripat orisent = match2 wild ps xs oripat orisent
{- TO BE WRITTEN -}
longerWildcardMatch (wc:ps) (x:xs) wild oripat orisent = match2 wild (wc:ps) xs oripat orisent
{- TO BE WRITTEN -}

extractJust :: Eq a => a -> [a] -> [a] -> [a] -> [a]
extractJust wild pat sent saved
  | pat == [] && sent == [] = saved
  | (pat !! 0 /= wild) && ((pat !! 0) == (sent !! 0)) = extractJust wild (tail pat) (tail sent) saved
  | isJust (match2 wild (tail pat) (tail sent) pat sent) = saved ++ [(head sent)]
  | otherwise = extractJust wild pat (tail sent) saved ++ [(head sent)]

-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}


