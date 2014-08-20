module Main where
import System.Environment
import qualified Data.Map as Map
import Text.Regex
import Control.Monad.Random
import Control.Applicative
    
type Transitions = Map.Map String Integer 
type Markov = Map.Map String Transitions
                
main :: IO ()
main = do
  input <- getContents
  sizestr:_ <- getArgs
  story <- evalRandIO $ generate (constructMarkov (tokenize input)) (read sizestr)
  putStrLn story
  
tokenize :: String -> [String]
tokenize s =
  splitRegex (mkRegex "\\s+") s

-- |Adds a word transition to the markov chain
addWord :: Markov -> String -> String -> Markov
addWord m from to =
    Map.alter (markovInsert to) from m 

-- |Used to insert into the markov map
markovInsert :: String -> Maybe Transitions -> Maybe Transitions
markovInsert next Nothing = Just $ Map.singleton next 1
markovInsert next (Just transitions) =
    Just $ Map.insertWith (\newval oldval -> newval + oldval) next 1 transitions

-- |Constructs a markov chain from a list of words.
constructMarkov :: [String] -> Markov
constructMarkov words =
    foldl (\m (from, to) -> addWord m from to) Map.empty (transitionList words)
          
transitionList :: [String] -> [(String, String)]
transitionList words = zip words (tail words)

randomStartWord :: RandomGen g => Markov -> Rand g String
randomStartWord markov = (words !!) <$> index
    where (words, _)  = unzip $ Map.toList markov
          index = getRandomR (0, (length words) - 1)

nextWord :: RandomGen g => Transitions -> Rand g String
nextWord transitions = fromList
                       (map
                        (\(word, weight) ->
                             (word, toRational weight))
                        (Map.toList transitions))
                       
buildString :: RandomGen g => Markov -> String -> Int -> Rand g String
buildString _ _ 0 = return ""
buildString markov word num =
  case Map.lookup word markov of
    Nothing -> return ""
    Just trans -> do
      next <- nextWord trans
      rest <- buildString markov next (num - 1)
      return (next ++ " " ++ rest)
  
generate :: RandomGen g => Markov -> Int -> Rand g String
generate markov len = do
  start <- randomStartWord markov
  buildString markov start len
