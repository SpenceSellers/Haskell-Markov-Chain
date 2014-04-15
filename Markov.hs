module Main where
import qualified Data.Map as Map
import Text.Regex

data Transition = Transition String Integer deriving Show

type Markov = Map.Map String [Transition]
                
main :: IO ()
main = do
  input <- getContents
  
  putStrLn $ show $ tokenize input

tokenize :: String -> [String]
tokenize s =
  splitRegex (mkRegex " ") s

-- Adds a word transition to the markov chain
addWord :: Markov -> String -> String -> Markov
addWord m from to =
    Map.alter (markovInsert to) from m 

-- Used to insert into the markov map
markovInsert :: String -> Maybe [Transition] -> Maybe [Transition]
-- If the word doesn't exist yet:
markovInsert next Nothing = Just [Transition next 1]
markovInsert next trns = 
