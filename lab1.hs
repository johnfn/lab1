import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Ord
import Control.Arrow
import System.Environment

data HistogramKey = Key (String, Int) deriving (Show)

--TODO
getName (Key (name, _)) = name
getValue (Key (_, value)) = value

maxWidth = 80

separation = " : "

strToLower = map toLower

stripPunctuation "" = ""
stripPunctuation word 
  | startsWithPunct word           = stripPunctuation $ tail word
  | startsWithPunct $ reverse word = stripPunctuation $ init word
  | otherwise = word
  where
    punctuation = "!@#$%^&*()_+-={}|[]\\:\";'<>?,./"
    startsWithPunct str = any id (map (== head str) punctuation) 

toLowerWords str = filter (/= "") $ map (stripPunctuation . strToLower) (words str)

padWord word padLength = word ++ replicate (padLength - length word) ' '

numXs count highestCount longestWord = 
  if highestCount < biggestNumOfXs 
  then count 
  else floor $ (toRational count / (toRational highestCount / toRational biggestNumOfXs))
  where
    biggestNumOfXs = maxWidth - longestWord - length separation

showKey longestWord highestCount (Key (name, count)) = 
  padWord name longestWord ++ separation ++ replicate (numXs count highestCount longestWord) 'X'

showHistogram pairs = do
  let longestWord = length $ maximumBy (comparing length) (map getName pairs)
  let largestKey = maximum (map getValue pairs)
  putStrLn $ intercalate "\n" $ map (showKey longestWord largestKey) pairs

main = do
  args <- getArgs

  contents <- 
    fmap toLowerWords $ 
    if null args
      then getContents
      else fmap (intercalate "") $ mapM readFile args

  let countPairs = map Key $ sortBy (comparing snd) $ map (head &&& length) $ group $ sort contents
  showHistogram countPairs
