module Huffman ( tree, charSequence, decode )
  where
    import Data.List
    import Data.Function (on)
    import Node
    import qualified Data.Map as Map
    import Data.Maybe
    import Debug.Trace
    import Data.Char (chr)

    pEOF = chr 999

    -- Create initial nodes (forest of trees) for each symbol
    createNodes :: String -> [Node]
    createNodes s = map (\ch -> createLeaf ch $ charWeight s ch) $ nub s

    -- Coding table generation step (recursive)
    step :: [Node] -> Node
    step nodes = let cut = (tail . tail) sorted
                 in
                  if length nodes > 1 then
                    step (merge:cut)
                  else
                    head nodes
                 where
                   sorted = sortBy (compare `on` weight) nodes
                   merge = Node { symbol = Nothing, weight = sumWeights, left = Just first, right = Just second }
                   first = head sorted
                   second = (head . tail) sorted
                   sumWeights = weight first + weight second
    tree :: String -> Node
    tree = step . createNodes

    -- Root-to-leaf search, find a character's sequence string
    charSequence :: Node -> Char -> String
    charSequence node ch =
      fromJust $ helper node ch ""
      where
        helper Node { symbol = s, left = l, right = r } ch sequ
          | s == Just ch = Just sequ
          | isNothing l && isNothing r       = Nothing
          | otherwise                        = let leftPath = helper (fromJust l) ch (sequ ++ "0")
                                                   rightPath = helper (fromJust r) ch (sequ ++ "1")
                                               in if isJust leftPath then leftPath else rightPath

    -- Root-to-leaf search, find a character based on sequence string
    findChar :: Node -> String -> Maybe Char
    findChar n@Node { symbol = s, left = l, right = r} sequ
      | not $ null sequ = let path = if head sequ == '0' then l else r
                          in if isJust path then
                              findChar (fromJust path) (tail sequ)
                             else
                               Nothing

      | otherwise = if isJust s then s else Nothing


    -- Encode an string into huffman coding
    encode :: String -> String
    encode input = let t = tree input
                       table = charTable t input
                   in concatMap (\a -> fromJust $ Map.lookup a table) input

    -- Character table, a Map representing each character's bit sequence
    charTable :: Node -> String -> Map.Map Char String
    charTable t input = Map.fromList $ map (\a -> (a, charSequence t a)) (nub input)

    -- Each character's weight in string
    charWeight :: String -> Char -> Float
    charWeight s x = genericLength $ filter (==x) s

    -- Decode a string, given the tree representing it
    decode :: Node -> String -> String
    decode t input = let (valid, next) = span (isNothing . findChar t) $ inits input
                         sequ = head next
                         ninput = tails input !! length valid
                         ch = fromJust $ findChar t sequ
                     in if not $ null ninput then
                          ch : decode t ninput
                        else
                          [ch]
