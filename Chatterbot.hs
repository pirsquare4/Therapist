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
stateOfMind brain  = do
  r <- randomIO :: IO Float
  return (rulesApply [map2 (id, pick r) rule | rule <- brain])

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply rules sentence 
  | sent == sentence = []
  | otherwise  = sent
    where
      sent = (try . transformationsApply "*" reflect ) rules sentence

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect sentence = [replaceWord word reflections | word <- sentence]


replaceWord :: String -> [(String, String)] ->  String
replaceWord word [] = word
replaceWord word (x:xs)
  | word == fst x = snd x
  | otherwise = replaceWord word xs

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
rulesCompile = map  (map2 (words . map  toLower, map words))


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
reductionsApply rules phrase
   |phrase == transform = phrase
   |otherwise = reductionsApply rules transform
   where
    transform = try (transformationsApply "*" id rules) phrase


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute wildcard t s = concat [ replaceList x wildcard s| x <- t]

-- Helper function for substitute
replaceList :: Eq a => a -> a -> [a] -> [a]
replaceList b c d 
  | b == c    = d
  | otherwise = [b]

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
-- match :: Eq a => a -> [a] -> [a] -> Maybe [a]
-- match wild pat sent = match2 wild pat sent pat sent

-- match2 :: Eq a => a -> [a] -> [a] -> [a] ->[a]-> Maybe [a]
-- match2 wild pat sent oripat orisent
--   | null pat && null sent = Just (extractJust wild oripat orisent [])
--   | null pat = Nothing
--   | null sent = Nothing
--   | (head pat /= wild) && (head pat /= head sent) = Nothing
--   | (head pat /= wild) && (head pat == head sent) = match2 wild (tail pat) (tail sent) oripat orisent
--   | isJust (singleWildcardMatch pat sent wild oripat orisent ) = singleWildcardMatch pat sent wild oripat orisent
--   | isJust(longerWildcardMatch pat sent wild oripat orisent) = longerWildcardMatch pat sent wild oripat orisent
--   | otherwise = Nothing

-- Helper function to match
-- singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> a -> [a] -> [a]-> Maybe [a]

-- singleWildcardMatch (wc:ps) (x:xs) wild = match2 wild ps xs

-- longerWildcardMatch (wc:ps) (x:xs) wild = match2 wild (wc:ps) xs



-- Another helper function to match
-- extractJust :: Eq a => a -> [a] -> [a] -> [a] -> [a]
-- extractJust wild pat sent saved
--   | null pat && null sent = saved
--   | (head pat /= wild) && (head pat == head sent) = extractJust wild (tail pat) (tail sent) saved
--   | isJust (match2 wild (tail pat) (tail sent) pat sent) = saved ++ [head sent]
--   | otherwise = extractJust wild pat (tail sent) (saved ++ [head sent])

-- new elegant solution. Ignore previous working code.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match wild pat sent
          | head pat == head sent = match wild (tail pat) (tail sent)
          | head pat /= wild = Nothing
          | otherwise = orElse (singleWildcardMatch pat sent) (longerWildcardMatch pat sent)


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) (match wc ps xs)
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) (match wc (wc:ps) xs)


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
transformationApply wild func sent tuple
   | isJust matched = mmap (substitute wild (snd tuple)) (mmap func matched)
   | otherwise = Nothing
   where 
    matched = match wild (fst tuple) sent


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply wild func tupleList sent
   |isJust transform = transform
   |null (tail tupleList)  = Nothing
   |otherwise = transformationsApply wild func (tail tupleList) sent
   where
    transform = transformationApply wild func sent (head tupleList)

