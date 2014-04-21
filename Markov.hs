module Main where
import qualified Data.Map as Map
import Text.Regex
import System.Random
import Control.Monad.State

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
        in words !! (evalState (randNum (0, length words - 1)) rand)

randNum :: (Int, Int) -> State StdGen Int
randNum (min, max) = do
    gen <- get
    let (num, newgen) = randomR (min, max) gen
      in do
        put newgen
        return num
