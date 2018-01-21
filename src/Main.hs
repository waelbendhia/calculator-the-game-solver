{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

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
  | Sum
  | ShiftLeft
  | ShiftRight
  | AddToButtons Integer
  | Mirror
  | Store
  | Unstore
  | Powers Integer
  | Replace Integer
            Integer
  deriving (Show)

data GameState = GameState
  { remainingMoves :: Integer
  , goal :: Integer
  , currentValue :: Integer
  , actions :: [Action]
  , store :: Maybe Integer
  } deriving (Show)

addToAction :: Integer -> Action -> Action
addToAction n act =
  let transform m =
        if m < 0
          then m - n
          else m + n
  in case act of
       Add x -> Add $ transform x
       Multiply x -> Multiply $ transform x
       Insert x -> Insert $ transform x
       Divide x -> Divide $ transform x
       Powers x -> Powers $ transform x
       Replace x y -> Replace (transform x) (transform y)
       x -> x

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

applyAction :: Action -> GameState -> Maybe GameState
applyAction act g =
  let GameState {currentValue = x} = g
      replaceValue y =
        if y > 99999
          then Nothing
          else Just $
               g {remainingMoves = remainingMoves g - 1, currentValue = y}
  in case act of
       Add y -> replaceValue $ x + y
       Multiply y -> replaceValue $ x * y
       FlipSign -> replaceValue $ -x
       RemoveDigit -> replaceValue $ quot x 10
       Reverse -> replaceValue $ reverseInt x
       Insert y -> replaceValue $ x * (10 ^ (length $show y)) + y
       Replace n m -> (replaceInt n m x) >>= replaceValue
       Powers y -> replaceValue $ x ^ y
       Mirror -> (readMaybe $ show x ++ (reverse $ show x)) >>= replaceValue
       ShiftLeft ->
         (readMaybe $ tail (show x) ++ [head (show x)]) >>= replaceValue
       ShiftRight ->
         (readMaybe $ last (show x) : init (show x)) >>= replaceValue
       Sum ->
         replaceValue $
         (if x < 0
            then (*) (-1)
            else (*) 1) $
         toInteger $ sum $ map digitToInt $ filter isDigit $ show x
       Divide y ->
         if x `rem` y == 0
           then replaceValue $ x `quot` y
           else Nothing
       AddToButtons n ->
         Just $
         g
         { remainingMoves = remainingMoves g - 1
         , actions = map (addToAction n) (actions g)
         }
       Store ->
         case store g of
           Nothing -> Just $ g {store = Just x}
           Just s ->
             if x == s
               then Nothing
               else Just $g {store = Just x}
       Unstore ->
         case store g of
           Nothing -> Nothing
           Just s -> applyAction (Insert s) g

childStates :: GameState -> [(Action, GameState)]
childStates gs
  | remainingMoves gs == 0 = []
  | otherwise =
    actions gs >>=
    (\act ->
       case applyAction act gs of
         Just x -> [(act, x)]
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
    { remainingMoves = moves
    , goal = gl
    , currentValue = starting
    , actions = acts
    , store = Nothing
    }

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
             return $
               case x of
                 Store -> [Store, Unstore] ++ acts
                 _ -> x : acts
           Nothing -> do
             putStrLn "Could not parse actions"
             promptActions

actionFromString :: [Char] -> Maybe Action
actionFromString str
  | length str == 0 = Nothing
  | map toLower str == "store" = Just Store
  | map toLower str == "<shift" = Just ShiftLeft
  | map toLower str == "shift>" = Just ShiftRight
  | map toLower str == "reverse" = Just Reverse
  | map toLower str == "sum" = Just Sum
  | map toLower str == "mirror" = Just Mirror
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
  | head str == '^' = readMaybe (tail str) >>= Just . Powers
  | head str == '*' = readMaybe (tail str) >>= Just . Multiply
  | head str == '/' = readMaybe (tail str) >>= Just . Divide
  | (take 3 str) == "[+]" = readMaybe (drop 3 str) >>= Just . AddToButtons
  | otherwise = readMaybe str >>= Just . Insert
