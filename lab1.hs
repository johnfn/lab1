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

strToLower = map toLower

stripPunctuation word 
  | startsWithPunct word = stripPunctuation $ tail word
  | startsWithPunct $ reverse word = stripPunctuation $ init word
  | otherwise = word
  where
    punctuation = "!@#$%^&*()_+-={}|[]\\:\";'<>?,./"
    startsWithPunct str = any id (map (== head str) punctuation) 

toLowerWords str = map (stripPunctuation . strToLower) (words str)

padWord word padLength = word ++ replicate (padLength - length word) ' '

--TODO: 80 -> pass in value
numXs count highestCount = 
  if highestCount < 80 
  then count 
  else floor $ (toRational count / (toRational highestCount / 80))

showKey longestWord highestCount (Key (name, count)) = 
  padWord name longestWord ++ " : " ++ replicate (numXs count highestCount) 'X'

showHistogram :: [HistogramKey] -> IO ()
showHistogram pairs = do
  let longestWord = length $ maximumBy (comparing length) (map getName pairs)
  let largestKey = maximum (map getValue pairs)
  putStrLn $ intercalate "\n" $ map (showKey longestWord largestKey) pairs

-- 80 char max
main = do
  --args <- getArgs
  let args = ["test1"]

  contents <- 
    fmap toLowerWords $ 
    if null args
      then getContents
      else fmap (intercalate "") $ mapM readFile args

  let countPairs = map Key $ sortBy (comparing snd) $ map (head &&& length) $ group $ sort contents
  showHistogram countPairs
