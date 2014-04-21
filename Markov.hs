module Main where
import qualified Data.Map as Map
import Text.Regex
--import System.Random
import Control.Monad.State
import Control.Monad.Random

type Transitions = Map.Map String Integer 

type Markov = Map.Map String Transitions
                
main :: IO ()
main = do
  input <- getContents
  rgen <- getStdGen
  let markov = constructMarkov $ tokenize input
  putStrLn $ randomStartWord rgen markov
  
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

randomStartWord :: StdGen -> Markov -> String
randomStartWord rand markov =
    let (words, _)  = unzip $ Map.toList markov
        in words !! (evalRand (randNum (0, length words - 1)) rand)

randNum :: RandomGen g => (Int, Int) -> Rand g Int
randNum (min, max) = do
  rand <- getRandomR (min, max)
  return rand


nextWord :: RandomGen g => Transitions -> Rand g String
nextWord transitions = do
  word <- fromList (map (\(word, weight) -> (word, toRational weight)) (Map.toList transitions))
  return word

buildString :: RandomGen g => Markov -> String -> Int -> Rand g String
buildString _ _ 0 = return ""
buildString markov word num = do
  next <- nextWord (markov Map.! word)
  rest <- buildString markov next (num - 1)
  return (next ++ " " ++ rest)
  
generate :: RandomGen g => Markov -> Int -> Rand g String
generate markov len = do
  gen <- get
  let start = randomStartWord gen markov
  buildString markov start len
