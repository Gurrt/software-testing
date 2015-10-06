  module Exercise4 where
  
  import Data.Maybe
  import Data.Char

  type Position = (Row,Column)
    
  --Exercise 4: time aprox 7 hours (struggling with Haskell).

  delBlockN :: String ->Node-> Node
  delBlockN nameB node = (s, constraints s)
    where s = delBlockS nameB (fst node) 

  delBlockS :: String -> Sudoku -> Sudoku
  delBlockS [] s = s
  delBlockS nameB s = foldl delBlock s (take (length nameB) (findBlockCoordinates nameB))

  delBlock :: Sudoku -> (Row,Column) ->  Sudoku
  delBlock s (r,c) (x,y) = (foldl eraseS s (subBlockPosition s (r,c))) (x,y) 
  
  subBlockPosition :: Sudoku -> Position -> [Position]
  subBlockPosition s (r, c) = [(r',c') | r' <- (bl r), c' <- (bl c)]

  findBlockCoordinates:: String -> [(Row, Column)]
  findBlockCoordinates [] = []
  findBlockCoordinates (x:xs) = findBlk (toUpper ([x] !!0)) : findBlockCoordinates xs

  findBlk :: Char -> (Row, Column)
  findBlk c = fromJust (lookup c [('A',(1,1)), ('B',(1,6)), ('C', (1,9)),('D', (6,1)),('E', (6,6)),('F', (6,9)),('G', (9,1)), ('H', (9,6)),('I',(9,9))])

  -- nameB given letter to choose a block nodes 

  main :: String -> IO ()
  main nameB = do 
          [r] <- rsolveNs [emptyN]
          showNode r
          let sBlock = delBlockN nameB r
          s  <- genProblem sBlock
          showNode s

  --test: main "eI"
  -- +-------+-------+-------+
  -- |x      |      x|      x|
  -- |   A   |   B   |   C   |
  -- |       |       |       |
  -- +-------+-------+-------+
  -- |       |       |       |
  -- |   D   |   E   |   F   |
  -- |x      |      x|      x|
  -- +-------+-------+-------+
  -- |       |       |       |
  -- |  G    |   H   |   I   |
  -- |x      |     x |      x|
  -- +-------+-------+-------+


