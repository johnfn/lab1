import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Ord
import Control.Arrow
import System.Environment

data HistogramKey = Key (String, Int) deriving (Show)

getName (Key (name, _)) = name

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

showKey longest (Key (name, count)) = padWord name longest ++ " : " ++ replicate count 'X'

showHistogram :: [HistogramKey] -> IO ()
showHistogram pairs = do
  let longest = length $ maximumBy (comparing length) (map getName pairs)
  putStrLn $ intercalate "\n" $ map (showKey longest) pairs

-- 80 char max
main = do
  --args <- getArgs
  let args = ["test"]

  contents <- 
    fmap toLowerWords $ 
    if null args
      then getContents
      else fmap (intercalate "") $ mapM readFile args

  let countPairs = map Key $ sortBy (comparing snd) $ map (head &&& length) $ group $ sort contents
  showHistogram countPairs
