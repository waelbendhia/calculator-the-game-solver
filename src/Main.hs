module Main where

import Data.Char
import Data.List
import Data.String.Utils
import Text.Read

data Action
  = Add Int
  | Multiply Int
  | Insert Int
  | FlipSign
  | RemoveDigit
  | Divide Int
  | Reverse
  | Sum
  | ShiftLeft
  | ShiftRight
  | AddToButtons Int
  | Mirror
  | Store
  | Unstore
  | Inv10
  | Powers Int
  | Replace String
            String
  deriving (Show)

data Portal =
  Portal Int
         Int
  deriving (Show)

data GameState = GameState
  { remainingMoves :: Int
  , goal :: Int
  , currentValue :: Int
  , actions :: [Action]
  , store :: Maybe Int
  , portal :: Maybe Portal
  } deriving (Show)

transformPortal :: GameState -> GameState
transformPortal gs =
  case portal gs of
    Just p -> gs {currentValue = applyPortal p $ currentValue gs}
    _ -> gs

applyPortal :: Integral t => Portal -> t -> t
applyPortal (Portal entry exit) res
  | res <= 10 ^ entry = res
  | otherwise =
    applyPortal (Portal entry exit) $ remainder + toInsert * 10 ^ exit
  where
    toInsert = (truncate $ (toRational res) / 10 ^ entry) `mod` 10
    remainder =
      (res `mod` 10 ^ entry) +
      (truncate $toRational res / 10 ^ (entry + 1)) * 10 ^ entry

addToAction :: Int -> Action -> Action
addToAction n act =
  case act of
    Add x -> Add $ add x
    Multiply x -> Multiply $ add x
    Insert x -> Insert $ add x
    Divide x -> Divide $ add x
    Powers x -> Powers $ add x
    Replace x y -> Replace (addStr x) (addStr y)
    x -> x
  where
    addStr = show . add . read
    add m
      | m < 0 = m - n
      | otherwise = m + n

replaceInt :: (Show a2, Read a1) => String -> String -> a2 -> Maybe a1
replaceInt from with = maybeRead . (replace from with) . show

reverseInt :: Integral t => t -> t
reverseInt n = aux n 0
  where
    aux 0 y = y
    aux x y =
      let (x', y') = x `quotRem` 10
      in aux x' (10 * y + y')

verifyValue :: GameState -> Maybe GameState
verifyValue gs
  | currentValue gs > 99999 = Nothing
  | otherwise = Just gs

replaceValue :: GameState -> Int -> Maybe GameState
replaceValue gs y =
  Just gs {currentValue = y, remainingMoves = remainingMoves gs - 1}

sumNumber :: (Show a, Num a, Ord a) => a -> Int
sumNumber n = sign $ sum $ map digitToInt $ filter isDigit $ show n
  where
    sign
      | n < 0 = (* (-1))
      | otherwise = id

invertNumbers :: (Show a2, Read a1) => a2 -> a1
invertNumbers n = read $ show n >>= flipDigit
  where
    flipDigit c
      | isDigit c = show $ (10 - digitToInt c) `mod` 10
      | otherwise = [c]

mirror :: (Show a2, Read a1) => a2 -> Maybe a1
mirror x = readMaybe $ show x ++ (reverse $ show x)

shift :: (Show a2, Read a1) => Bool -> a2 -> Maybe a1
shift left x =
  readMaybe $
  if left
    then tail (show x) ++ [head (show x)]
    else last (show x) : init (show x)

divide :: Integral a => a -> a -> Maybe a
divide y x
  | x `rem` y == 0 = Just $ x `quot` y
  | otherwise = Nothing

applyAction :: Action -> GameState -> Maybe GameState
applyAction act g =
  let GameState {currentValue = x} = g
      out =
        case act of
          Add y -> replaceValue g $ x + y
          Multiply y -> replaceValue g $ x * y
          FlipSign -> replaceValue g $ -x
          RemoveDigit -> replaceValue g $ quot x 10
          Reverse -> replaceValue g $ reverseInt x
          Insert y -> replaceValue g $ x * (10 ^ (length $show y)) + y
          Powers y -> replaceValue g $ x ^ y
          Sum -> replaceValue g $ sumNumber x
          Inv10 -> replaceValue g $ invertNumbers x
          Replace n m -> (replaceInt n m x) >>= replaceValue g
          Mirror -> mirror x >>= replaceValue g
          ShiftLeft -> shift True x >>= replaceValue g
          ShiftRight -> shift False x >>= replaceValue g
          Divide y -> divide y x >>= replaceValue g
          AddToButtons n ->
            Just $
            g
            { remainingMoves = remainingMoves g - 1
            , actions = map (addToAction n) $ actions g
            }
          Store ->
            case store g of
              Nothing -> Just $ g {store = Just x}
              Just s ->
                if x /= s
                  then Just $ g {store = Just x}
                  else Nothing
          Unstore -> store g >>= (flip applyAction) g . Insert
  in out >>= verifyValue >>= Just . transformPortal

childStates :: GameState -> [(Action, GameState)]
childStates gs
  | remainingMoves gs == 0 || currentValue gs == goal gs = []
  | otherwise = actions gs >>= actionToState
  where
    actionToState act =
      case applyAction act gs of
        Just x -> [(act, x)]
        Nothing -> []

data Node =
  Node (Maybe (Node, Action))
       GameState

actionChain :: Node -> [Action]
actionChain (Node parent _) =
  case parent of
    Nothing -> []
    Just (n, a) -> actionChain n ++ [a]

solveBfs :: GameState -> Maybe [Action]
solveBfs startState = bfs [Node Nothing startState]
  where
    bfs [] = Nothing
    bfs (x:xs)
      | goal cur == currentValue cur = Just $ actionChain x
      | remainingMoves cur == 0 = bfs xs
      | otherwise = bfs $ xs ++ childNodes
      where
        (Node _ cur) = x
        toNode (action, child) = Node (Just (x, action)) child
        childNodes = map toNode $ childStates cur

main :: IO ()
main = promptGameState >>= putStrLn . prettyPrint . solveBfs >>= always main

promptGameState :: IO GameState
promptGameState = do
  putStrLn "Enter game state:"
  moves <- promptInt "Moves: "
  gl <- promptInt "Goal: "
  starting <- promptInt "Starting value: "
  prtl <- promptPortal
  acts <- promptActions
  return
    GameState
    { remainingMoves = moves
    , goal = gl
    , currentValue = starting
    , actions = acts
    , store = Nothing
    , portal = prtl
    }

prettyPrint :: Maybe [Action] -> String
prettyPrint Nothing = "Unsolvable"
prettyPrint (Just l) =
  "\nSolution: " ++ (concat $ intersperse " -> " $ map show l) ++ "\n"

promptLine :: String -> IO String
promptLine prompt = putStrLn prompt >>= always getLine

promptPortal :: IO (Maybe Portal)
promptPortal =
  promptLine "Portal none for no portal or 'entry' 'exit':" >>= checkInput
  where
    checkInput ln
      | ln == "none" = return Nothing
      | length sep == 2 =
        return $ do
          entry' <- entry
          exit' <- exit
          return $ Portal entry' exit'
      | otherwise = retry promptPortal
      where
        sep = split " " ln
        [entry, exit] = map readMaybe sep

always :: p1 -> p2 -> p1
always a _ = a

retry :: IO b -> IO b
retry a = putStrLn "Could not parse input:" >>= always a

promptInt :: String -> IO Int
promptInt prompt = do
  ln <- promptLine prompt
  case readMaybe ln of
    Just x -> return x
    Nothing -> retry $ promptInt prompt

promptActions :: IO [Action]
promptActions = promptLine "Action or done for done" >>= verify
  where
    verify ln
      | ln == "done" = return []
      | otherwise =
        case actionFromString ln of
          Just x ->
            promptActions >>=
            return .
            case x of
              Store -> (++) [Store, Unstore]
              _ -> (:) x
          Nothing -> retry promptActions

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
        [n, m] = operands
    in if length operands == 2
         then do
           _ <- readMaybe n :: Maybe Int
           _ <- readMaybe m :: Maybe Int
           return $ Replace n m
         else Nothing
  | head str == '-' = readMaybe str >>= Just . Add
  | head str == '+' = readMaybe (tail str) >>= Just . Add
  | head str == '^' = readMaybe (tail str) >>= Just . Powers
  | head str == '*' = readMaybe (tail str) >>= Just . Multiply
  | head str == '/' = readMaybe (tail str) >>= Just . Divide
  | (take 3 str) == "[+]" = readMaybe (drop 3 str) >>= Just . AddToButtons
  | otherwise = readMaybe str >>= Just . Insert
