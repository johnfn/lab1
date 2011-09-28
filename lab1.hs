import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Ord
import System.Environment

toLowerWords str = map (map toLower) (words str)
sortPairs = sortBy $ comparing snd
showHistogram pairs = intercalate "\n" (map (\pair -> replicate (snd pair) 'X' ++ " : " ++ fst pair) pairs)

-- multiple file readin
main = do
  args <- getArgs

  contents <- 
    fmap toLowerWords $ 
    if null args
      then getContents
      else fmap (intercalate "") $ mapM readFile args

  let histogramData = Map.toList $ Map.fromListWith (+) (zip contents (repeat 1))
  putStrLn $ showHistogram $ sortPairs histogramData
