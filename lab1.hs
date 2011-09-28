import qualified Data.Map as Map
import Data.Char
import Data.List
import System.Environment

toLowerWords str = map (map toLower) (words str)
sortPairs pairs = sortBy (\pairA pairB -> compare (snd pairA) (snd pairB)) pairs
showHistogram pairs = intercalate "\n" (map (\pair -> (take (snd pair) (repeat 'X')) ++ " : " ++ (fst pair)) pairs)

-- multiple file readin
main = do
  args <- getArgs

  contents <- 
    fmap (toLowerWords) $ 
    if null args
      then getContents
      else fmap (intercalate "") $ sequence $ map readFile args

  let histogramData = Map.toList $ Map.fromListWith (+) (zip contents (repeat 1))
  putStrLn $ showHistogram $ sortPairs histogramData
