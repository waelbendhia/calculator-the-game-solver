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
  | Inv10
  | Powers Integer
  | Replace String
            String
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
        if | m < 0 -> m - n
           | otherwise -> m + n
  in case act of
       Add x -> Add $ transform x
       Multiply x -> Multiply $ transform x
       Insert x -> Insert $ transform x
       Divide x -> Divide $ transform x
       Powers x -> Powers $ transform x
       Replace x y ->
         Replace (show $transform $ read x) (show $ transform $read y)
       x -> x

replaceInt :: (Show a2, Read a1) => String -> String -> a2 -> Maybe a1
replaceInt from with = maybeRead . (replace from with) . show

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
        if | y > 99999 -> Nothing
           | otherwise ->
             Just $ g {remainingMoves = remainingMoves g - 1, currentValue = y}
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
         (*)
           (if | x < 0 -> -1
               | otherwise -> 1) $
         toInteger $ sum $ map digitToInt $ filter isDigit $ show x
       Divide y ->
         if | x `rem` y == 0 -> replaceValue $ x `quot` y
            | otherwise -> Nothing
       AddToButtons n ->
         Just $
         g
         { remainingMoves = remainingMoves g - 1
         , actions = map (addToAction n) (actions g)
         }
       Inv10 ->
         replaceValue $
         read $
         show x >>= \c ->
           if | isDigit c -> show $ mod (10 - digitToInt c) 10
              | otherwise -> [c]
       Store ->
         case store g of
           Nothing -> Just $ g {store = Just x}
           Just s ->
             if | x == s -> Nothing
                | otherwise -> Just $ g {store = Just x}
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
  if | ln == "done" -> return []
     | otherwise ->
       case actionFromString ln of
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
  | map toLower str == "inv10" = Just Inv10
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
    in if | length operands == 2 ->
            (let [n, m] = operands
             in (do _ <- readMaybe n :: Maybe Integer
                    _ <- readMaybe m :: Maybe Integer
                    return (Replace n m)))
          | otherwise -> Nothing
  | head str == '-' = readMaybe str >>= Just . Add
  | head str == '+' = readMaybe (tail str) >>= Just . Add
  | head str == '^' = readMaybe (tail str) >>= Just . Powers
  | head str == '*' = readMaybe (tail str) >>= Just . Multiply
  | head str == '/' = readMaybe (tail str) >>= Just . Divide
  | (take 3 str) == "[+]" = readMaybe (drop 3 str) >>= Just . AddToButtons
  | otherwise = readMaybe str >>= Just . Insert
