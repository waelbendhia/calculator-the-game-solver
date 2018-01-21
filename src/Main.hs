{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Char
import Data.List
import Data.String.Utils
import Text.Read

data Action
  = Add Integer
  | Multiply Integer
  | Insert Integer
  | FlipSign
  | RemoveDigit
  | Divide Integer
  | Reverse
  | Replace Integer
            Integer
  deriving (Show)

data GameState = GameState
  { remainingMoves :: Integer
  , goal :: Integer
  , currentValue :: Integer
  , actions :: [Action]
  } deriving (Show)

replaceInt :: (Show a4, Show a3, Show a2, Read a1) => a2 -> a3 -> a4 -> Maybe a1
replaceInt from with val =
  maybeRead $ replace (show from) (show with) (show val)

reverseInt :: Integral t => t -> t
reverseInt n = aux n 0
  where
    aux 0 y = y
    aux x y =
      let (x', y') = x `quotRem` 10
      in aux x' (10 * y + y')

applyAction :: Action -> Integer -> Maybe Integer
applyAction act x =
  let res =
        case act of
          Add y -> Just $ x + y
          Multiply y -> Just $ x * y
          FlipSign -> Just $ -x
          RemoveDigit -> Just $ quot x 10
          Reverse -> Just $ reverseInt x
          Insert y -> Just $ x * (10 ^ (length $show y)) + y
          Replace n m -> replaceInt n m x
          Divide y ->
            if x `rem` y == 0
              then Just $ x `quot` y
              else Nothing
  in case res of
       Just y ->
         if y > 999999
           then Nothing
           else Just y
       Nothing -> Nothing

childStates :: GameState -> [(Action, GameState)]
childStates gs
  | remainingMoves gs == 0 = []
  | otherwise =
    (actions gs) >>=
    (\act ->
       case applyAction act (currentValue gs) of
         Just x ->
           [ ( act
             , (gs {remainingMoves = remainingMoves gs - 1, currentValue = x}))
           ]
         Nothing -> [])

solve :: GameState -> Maybe [Action]
solve gs
  | goal gs == currentValue gs = Just []
  | remainingMoves gs == 0 = Nothing
  | otherwise =
    foldl
      (\p (act, state) ->
         case p of
           Nothing ->
             case solve state of
                  Just x -> Just $ act : x
                  Nothing -> Nothing
           _ -> p)
      Nothing $
    childStates gs

main :: IO ()
main = do
  gs <- promptGameState
  putStrLn $ prettyPrint $ solve $ gs
  main

promptGameState :: IO GameState
promptGameState = do
  putStrLn "Enter game state:"
  moves <- promptInt "Moves: "
  gl <- promptInt "Goal: "
  starting <- promptInt "Starting value: "
  acts <- promptActions
  return
    GameState
    {remainingMoves = moves, goal = gl, currentValue = starting, actions = acts}

prettyPrint :: Maybe [Action] -> String
prettyPrint Nothing = "Unsolvable"
prettyPrint (Just []) = ""
prettyPrint (Just (x:xs)) = "\t" ++ show x ++ "\n" ++ (prettyPrint $ Just xs)

promptLine :: String -> IO String
promptLine prompt = do
  putStrLn prompt
  ln <- getLine
  return ln

promptInt :: String -> IO Integer
promptInt prompt = do
  ln <- promptLine prompt
  case readMaybe ln of
    Just x -> return x
    Nothing -> do
      putStrLn "Could not parse input."
      promptInt prompt

promptActions :: IO [Action]
promptActions = do
  ln <- promptLine "Action or done for done"
  if ln == "done"
    then return []
    else case actionFromString ln of
           Just x -> do
             acts <- promptActions
             return (x : acts)
           Nothing -> do
             putStrLn "Could not parse actions"
             promptActions

actionFromString :: [Char] -> Maybe Action
actionFromString str
  | length str == 0 = Nothing
  | map toLower str == "reverse" = Just Reverse
  | str == "+/-" = Just FlipSign
  | str == "<<" = Just RemoveDigit
  | isInfixOf "=>" str =
    let operands = split "=>" str
    in (if length operands == 2
          then (let [n, m] = map readMaybe operands
                in (do n' <- n
                       m' <- m
                       return (Replace n' m')))
          else Nothing)
  | head str == '-' = readMaybe str >>= Just . Add
  | head str == '+' = readMaybe (tail str) >>= Just . Add
  | head str == '*' = readMaybe (tail str) >>= Just . Multiply
  | head str == '/' = readMaybe (tail str) >>= Just . Divide
  | otherwise = readMaybe str >>= Just . Insert
