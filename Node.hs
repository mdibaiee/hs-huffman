module Node
( createLeaf, Node(..), prettyPrint ) where
  import qualified Data.Map as Map
  import Data.Maybe

  data Node = Node { symbol :: Maybe Char
                   , weight :: Float
                   , left :: Maybe Node
                   , right :: Maybe Node
                   } deriving (Show)

  createLeaf :: Char -> Float -> Node
  createLeaf c a = Node { symbol = Just c
                        , weight = a
                        , left = Nothing
                        , right = Nothing }

  prettyPrint :: Node -> Int -> String
  prettyPrint Node { weight = w, symbol = s, left = l, right = r } x =
          let spaces = replicate (x * 2) ' '
              indent = "\n" ++ spaces
              x' = x + 1
              ws = "Weight: " ++ show w
              ss = if isJust s then "Symbol: " ++ (show . fromJust) s else ""
              ls = if isJust l then indent ++ "Left: " ++ prettyPrint (fromJust l) x' else ""
              rs = if isJust r then indent ++ "Right: " ++ prettyPrint (fromJust r) x' else ""
          in ws ++ " " ++ ss ++ ls ++ rs
