-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module ReversiAI(State,author,nickname,initial,think) where

import Reversi
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- Description of solution
My solution for this assignment is not what I would call beautiful. It’s most likely heavy computationally wise,
and consists of some unnecessary steps, it was however, simple to code and does the job. 
The methodology I pursued for the algorithm was: 
1. 
Find the first possible legal move, from board slot 0 and up. 
2. 
Find which direction(s) that made the move legal. (Put these in a list of bools, where each index is a direction)
3. 
Put these direction(s) and the move in a function that finds the affected bricks (this is made easier in my algorithm 
because the AI already knows what direction(s) that it should search within, minimizing potential error cases. 
4. 
Put these affected bricks in the createNewBoard algorithm, which returns the new board with the affected bricks in the current player's color.  
Advantage of my algorithm:
- My AI will always make the same move for the given board and player. 
- The createNewBoard function can either have an already determined move as input or just input my makeMove function.
- The entire board is not searched to find a move, this is the worst case. My algorithm returns the first possible move,
   after that, the search is terminated. This is good for time complexity. 
- Chain of functions that are quite easily follow
- The actual function calls that are necessary for a player are few (makeMove and createNewBoard)
Disadvantage of my algorithm:
- Pseudo random moves (the first possible move will always be picked, even if it’s not a “good” move. 
- No search tree, miniMax algorithm etc. 
- The algorithm has some steps that could be removed. Instead of searching for a legal direction and then finding the affected bricks in the legal direction,
  it could easily do these two steps at the same time. 
-}

{- type representing the state of the board.
Data is represented in a tuple (board,player) where board is the current board in a game and player is the color my AI plays as.
-}
type State = (Board, Reversi.Player)

author :: String
author = "Karl Bylander"

nickname :: String
nickname = "unReversi"


{-initial player
initializing the game, returning what color my AI plays as and the initial board. 
-}
initial :: Reversi.Player -> State
initial White = (iniBoard, White)
initial Black = (iniBoard, Black)

{- think state move time 
Controlling the communication between my and the other AI. 
-}
think ::  ReversiAI.State -> Reversi.Move -> Double -> (Reversi.Move, ReversiAI.State)
think (board,White) move time = (makeMove (createNewBoard board Black move) White, (createNewBoard (createNewBoard board Black move) White (makeMove (createNewBoard board Black move) White), White))
think (board,Black) move time = (makeMove (createNewBoard board White move) Black, (createNewBoard (createNewBoard board White move) Black (makeMove (createNewBoard board White move) Black), Black))

{- type representing the board.
Data is represented as a list [Maybe player], where if no player in the given index, Nothing. 
If a player occupies index, then Just player. 
INVARIANT: a Board should be of size 64. The initial board should have all blocks as Nothing, 
except board slots 28 and 35 as Just Black and board slots 27 and 36 as Just White. 
-}
type Board = [Maybe Reversi.Player] 

{-type representing possible directions a move can influence. 
U: Up (Number wise when looking at the board, a step is +8 from where you are)
D: Down (number wise when looking at the board, a step is -8 from where you are)
R: Right (number wise when looking at the board, a step is +1 from where you are)
L: Left (number wise when looking at the board, a step is -1 from where you are)
UL: Up Left (number wise when looking at the board, a step is +7 from where you are)
UR: Up Right (number wise when looking at the board, a step is +9 from where you are)
DL: Down Left (number wise when looking at the board, a step is -9 from where you are)
DR: Down Right (number wise when looking at the board, a step is -7 from where you are)
-}
data Direction = U | D | R | L | UL | UR | DL | DR deriving (Show, Eq)

{-The initial board in accordance to othello rules.-}
iniBoard = 
  [
  Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
  Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
  Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
  Nothing, Nothing, Nothing, Just White, Just Black, Nothing, Nothing, Nothing,
  Nothing, Nothing, Nothing, Just Black, Just White, Nothing, Nothing, Nothing,
  Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
  Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing,
  Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing
  ]

{- findIndexColor board idx
Finds the color or Nothing of given idx on the board.
RETURNS: Just player or Nothing depending on if a player has a brick on idx or not. 
EXAMPLE:  findIndexColor iniBoard 35 = Just Black
          findIndexColor iniBoard 50 = Nothing
          *iniBoard is the starting board of Othello/Reversi.
-}

findIndexColor :: Board -> Int -> Maybe Reversi.Player
findIndexColor board index 
  | index < 0 || index > 63 = Nothing
  | otherwise = board !! index

{- findRow searchpoint 
Finds which row of the board current searchpoint is located. 
RETURNS: An Int list of all the numbers on the given row.
PRE: Length of board row is 8, and size of board is max 64, starting at 0. 
EXAMPLE:  findRow 3 = [0,1,2,3,4,5,6,7]
          findRow 46 = [40,41,42,43,44,45,46,47]
          findRow 64 = []
-}
findRow :: Int -> [Int]
findRow searchpoint 
  | searchpoint `elem` [0..7] = [0..7]
  | searchpoint `elem` [8..15] = [8..15]
  | searchpoint `elem` [16..23] = [16..23]
  | searchpoint `elem` [24..31] = [24..31]
  | searchpoint `elem` [32..39] = [32..39]
  | searchpoint `elem` [40..47] = [40..47]
  | searchpoint `elem` [48..55] = [48..55]
  | searchpoint `elem` [56..63] = [56..63]
  | otherwise = []

{- moveRamifications board player affecteddirections move acc
Produces the list, [Int], of bricks that will be changed due to given move and given list [Bool] symbolising if a direction on the board is influenced by the move. 
RETURNS: List of Int type that will change color to current player. 
PRE:  length [Bool] == 7 and every index corresponds to a direction (and if you want to follow the rules of Reversi, is a direction that will be affected by given move, given othello/reversi rules) 
      (Move int): 0 <= int <= (length board) -1
EXAMPLE:  moveRamifications iniBoard Black [False,False,True,False,False,False,False,False] (Move 19) 0 = [19,27]
          moveRamifications iniBoard White [False,True,False,False,False,False,False,False] (Move 34) 0 = [34,35]
          moveRamifications iniBoard White [False,False,False,False,False,False,False,False] Pass 0 = []
          *iniBoard is the start board of the game.
          *For the direction of index in affecteddirections see (identifyDirection :: Int -> Direction) function below.
-}
moveRamifications :: Board -> Reversi.Player -> [Bool] -> Reversi.Move -> Int ->[Int]

--VARIANT: Length affecteddirections
moveRamifications board player _ Pass _ = []
moveRamifications board player [] (Move move) acc = []
moveRamifications board player (x:xs) (Move move) acc
  | not x = moveRamifications board player xs (Move move) (acc+1)
  | x = (findBricks board player move (identifyDirection acc)) ++ (moveRamifications board player xs (Move move) (acc+1))

{- identifyDirection int
Identifies what Direction the integer corresponds to. Important in createNewBoard where a possible position in an list [Int] indicates whether a move will influence given Direction.
RETURNS: The Direction given Int corresponds to. 
PRE: 0 <= int <= 7
EXAMPLE:  identifyDirection 3 = D
          identifyDirection 7 = DR
-}
identifyDirection :: Int -> Direction
identifyDirection int 
  | int == 0 = L
  | int == 1 = R 
  | int == 2 = U
  | int == 3 = D
  | int == 4 = UL
  | int == 5 = DL 
  | int == 6 = UR 
  | int == 7 = DR

{- possibleMoves board current player
Finds the first possible legal move given the board in ascending order. 
RETURNS: The value of Int type representing the move my AI will make on the board, if no move is found -1 will be returned. 
PRE:  0 <= current
EXAMPLE:  possibleMoves iniBoard 0 White = 20
          possibleMoves iniBoard 0 Black = 19
          *iniBoard is the starting board of Othello/Reversi.
-}
possibleMoves :: Board -> Int -> Reversi.Player -> Int

--VARIANT: Length of board. 
possibleMoves board current player
  | findIndexColor board current /= Nothing = possibleMoves board (current+1) player
  | current > (length board -1) = -1
  | isLegal board player current U 
  || isLegal board player current D 
  ||isLegal board player current L 
  || isLegal board player current R 
  || isLegal board player current UL 
  || isLegal board player current DL 
  || isLegal board player current UR 
  || isLegal board player current DR = current
  | otherwise = possibleMoves board (current+1) player

{- makeMove board player 
Returns the move my AI will make, given the board and player. 
RETURNS: The deterministic move my AI will do given the input board and player, returns Pass if (and only if) no move is possible. 
PRE: -1 is not a slot on the board. (Move int), int <=63
EXAMPLE:  makeMove [Nothing, Just White, Just Black] Black = Move 0
          makeMove [Nothing, Just White, Just Black] White = Pass
-}

makeMove :: Board -> Reversi.Player -> Reversi.Move
makeMove board player
  | move == (-1) = Pass
  | otherwise = Move move
 where move = possibleMoves board 0 player

{- createNewBoard board player move
Creates a new board given a new move. 
RETURNS: A new board, like the input board but the affected bricks are in player's color, if Pass then output is the input board. 
PRE: (Move int): 0 <= int <= 64 - 1
      move is a legal move, in accorance to Othello/Reversi rules. 
EXAMPLE:  createNewBoard Black (Move 26) = board but bricks 26 and 27 are also black. 
          createNewBoard Black Pass = iniBoard
          *iniBoard is the starting board of Othello/Reversi.
-}
createNewBoard :: Board -> Reversi.Player -> Reversi.Move -> Board
createNewBoard board player Pass = board
createNewBoard board player (Move move) = createNewBoard' board (Just player) (sortAndRemoveDupl (moveRamifications board player (map (isLegal board player move) [L,R,U,D,UL,DL,UR,DR]) (Move move) 0)) 0

{-Helper function to createNewBoard, see above for description. -}
createNewBoard' :: Board -> Maybe Reversi.Player -> [Int] -> Int -> Board

--VARIANT: length of board
createNewBoard' (s:ss) player [] acc = s : createNewBoard' (ss) player [] (acc+1)
createNewBoard' [] player [] acc = []
createNewBoard' (s:ss) player (x:xs) acc 
  | acc == 63 && x == acc = [player]
  | x == acc = player : createNewBoard' (ss) player (xs) (acc+1)
  | otherwise = s : createNewBoard' (ss) player (x:xs) (acc+1)

{- isLegal board player searchpoint direction 
Validates if a given move is a legal move for a given direction. 
RETURNS: True, if for a given move and direction it is legal or False if not. 
EXAMPLE:  isLegal iniBoard Black 19 U = True
          isLegal iniBoard Black 19 U = False
          isLegal iniBoard White 34 R = True
          *iniBoard is the starting board of Othello/Reversi.
-}
isLegal :: Board -> Reversi.Player -> Int -> Direction -> Bool
isLegal board player searchpoint L = horiIsLegal board (Just player) searchpoint (findRow searchpoint) 0 (-1)
isLegal board player searchpoint R = horiIsLegal board (Just player) searchpoint (findRow searchpoint) 0 1
isLegal board player searchpoint U = vertIsLegal board (Just player) searchpoint 0 8
isLegal board player searchpoint D = vertIsLegal board (Just player) searchpoint 0 (-8)
isLegal board player searchpoint UL = diagIsLegal board (Just player) searchpoint (findRow searchpoint) 0 7
isLegal board player searchpoint DL = diagIsLegal board (Just player) searchpoint (findRow searchpoint) 0 (-9)
isLegal board player searchpoint UR = diagIsLegal board (Just player) searchpoint (findRow searchpoint) 0 9
isLegal board player searchpoint DR = diagIsLegal board (Just player) searchpoint (findRow searchpoint) 0 (-7)

{- diagIsLegal board player searchpoint row acc movedirection
Handles the legality of given diagonal direction and searchpoint, Most easily applied using parent function isLegal and then specifying the direction.
RETURNS: True if the move is legal in regards to the movedirection, False if not.
PRE:  if movedirection is:  UL (upper left) = 7
                            DL (down left) = (-9)
                            UR (upper right) = 9
                            DR (downright) = (-7)
EXAMPLE:  diagIsLegal iniBoard (Just White) 34 [32..39] 0 7 = False
          *iniBoard is the starting board of Othello/Reversi.
-}

diagIsLegal :: Board -> Maybe Reversi.Player -> Int -> [Int] -> Int -> Int -> Bool

--VARIANT: Number of possible diagonal moves.
diagIsLegal board player searchpoint row acc movedirection 
  | searchpoint > 63 || searchpoint < 0 = False
  | findIndexColor board searchpoint == Nothing && acc /= 0 = False
  | acc <= 1 && findIndexColor board searchpoint == player = False
  | findIndexColor board searchpoint == player = True 
  | lastorfirst searchpoint movedirection == searchpoint = False
  | otherwise = diagIsLegal board player (searchpoint+movedirection) row (acc+1) movedirection

{- horiIsLegal board player searchpoint row acc movedirection
Handles the legality of given horisontal direction and searchpoint, Most easily applied using parent function isLegal and then specifying the direction.
RETURNS: True if the move is legal in regards to the movedirection, False if not.
PRE:   if movedirection is: R (right) = 1
                            L (left) = -1
EXAMPLE:  horiIsLegal iniBoard (Just White) 34 [32..39] 0 (-1) = False
          horiIsLegal iniBoard (Just White) 34 [32..39] 0 1 = True
          *iniBoard is the starting board of Othello/Reversi.
-}
horiIsLegal :: Board -> Maybe Reversi.Player -> Int -> [Int] -> Int -> Int -> Bool

--VARIANT: number of columns.
horiIsLegal board player searchpoint row acc movedirection
  | not (elem searchpoint row) = False
  | findIndexColor board searchpoint == Nothing && acc /= 0 = False
  | acc <= 1 && findIndexColor board searchpoint == player = False
  | findIndexColor board searchpoint == player = True 
  | otherwise = horiIsLegal board player (searchpoint+movedirection) row (acc+1) movedirection

{- vertIsLegal board player searchpoint acc movedirection
Handles the legality of given vertical direction and searchpoint, Most easily applied using parent function isLegal and then specifying the direction.
RETURNS: True if the move is legal in regards to the movedirection, False if not.
PRE: if movedirection is:  U (upwards) = 8
                           D (downwards) = -8
EXAMPLE:  vertIsLegal iniBoard (Just Black) 19 0 (-8) = False
          vertIsLegal iniBoard (Just Black) 19  0 8 = True
          *iniBoard is the starting board of Othello/Reversi. 
-}
vertIsLegal :: Board -> Maybe Reversi.Player -> Int -> Int -> Int -> Bool

--VARIANT: number of rows.
vertIsLegal board player searchpoint acc movedirection
  | searchpoint > 63 || searchpoint < 0 = False
  | findIndexColor board searchpoint == Nothing && acc /= 0 = False
  | acc <= 1 && findIndexColor board searchpoint == player = False
  | findIndexColor board searchpoint == player = True 
  | otherwise = vertIsLegal board player (searchpoint+movedirection) (acc+1) movedirection

{- findBricks board player searchpoint direction 
Finds the bricks that will change to player's color, given the direction and searchpoint. 
RETURNS: The list of Int type that will change to player color, given the direction and searchpoint
PRE: direction is a legal direction (i.e. the direction inserted will result in a changed color)
EXAMPLE:  findBricks iniBoard Black 19 U = [19,27]
          findBricks iniBoard White 34 R = [34,35]          
          *iniBoard is the starting board of Othello/Reversi.
-}
findBricks :: Board -> Reversi.Player -> Int -> Direction -> [Int]
findBricks board player searchpoint L = findHori board (Just player) searchpoint (findRow searchpoint) 0 (-1)
findBricks board player searchpoint R = findHori board (Just player) searchpoint (findRow searchpoint) 0 1
findBricks board player searchpoint U = findVert board (Just player) searchpoint 0 8
findBricks board player searchpoint D = findVert board (Just player) searchpoint 0 (-8)
findBricks board player searchpoint UL = findDiag board (Just player) searchpoint (findRow searchpoint) 0 7
findBricks board player searchpoint DL = findDiag board (Just player) searchpoint (findRow searchpoint) 0 (-9)
findBricks board player searchpoint UR = findDiag board (Just player) searchpoint (findRow searchpoint) 0 9
findBricks board player searchpoint DR = findDiag board (Just player) searchpoint (findRow searchpoint) 0 (-7)


{- findDiag board player searchpoint row acc movedirection
Finds the affected bricks given the searchpoint and direction (diagonal). Given that the direction is a legal direction. Most easily applied using parent function isLegal and then specifying the direction.
RETURNS: List of bricks that a given move will influence, given the diagonal direction. 
PRE:  movedirection is a legal direction (i.e. the direction inserted will result in a changed color)
      if movedirection is:  UL (upper left) = 7
                            DL (down left) = (-9)
                            UR (upper right) = 9
                            DR (downright) = (-7)
EXAMPLE:  *similarily to findHori but diagonally. 
          *iniBoard is the starting board of Othello/Reversi.
-}
findDiag :: Board -> Maybe Reversi.Player -> Int -> [Int] -> Int -> Int -> [Int]

--VARIANT: Number of possible diagonalmoves.
findDiag board player searchpoint row acc int 
  | searchpoint > 63 || searchpoint < 0 = []
  | findIndexColor board searchpoint == Nothing && acc /= 0 = []
  | acc <= 1 && findIndexColor board searchpoint == player = []
  | findIndexColor board searchpoint == player = [] 
  | lastorfirst searchpoint int == (searchpoint+int) = []
  | otherwise = searchpoint : findDiag board player (searchpoint+int) row (acc+1) int

{- findHori board player searchpoint row acc movedirection
Finds the affected bricks given the searchpoint and direction (horisontal). Given that the direction is a legal direction. Most easily applied using parent function findBricks and then specifying the direction.
RETURNS: List of bricks that a given move will influence, given the horisontal direction. 
PRE:  movedirection is a legal direction (i.e. the direction inserted will result in a changed color)
      if movedirection is:  R (right) = 1
                            L (left) = -1
EXAMPLE:  findHori iniBoard (Just White) 34 [32..39] 0 1 = [34,35]
          findHori iniBoard (Just Black) 26 [24..31] 0 1 = [26,27]
          *iniBoard is the starting board of Othello/Reversi.
          *[24..31] or [32..39] are lists from findRow for given searchpoint. 
-}
findHori :: Board -> Maybe Reversi.Player -> Int -> [Int] -> Int -> Int -> [Int]

--VARIANT: number of columns.
findHori board player searchpoint row acc int
  | not (elem searchpoint row) = []
  | findIndexColor board searchpoint == Nothing && acc /= 0 = []
  | acc <= 1 && findIndexColor board searchpoint == player = []
  | findIndexColor board searchpoint == player = [] 
  | otherwise = searchpoint : findHori board player (searchpoint+int) row (acc+1) int

{- findVert board player searchpoint acc movedirection
Finds the affected bricks given the searchpoint and direction (vertical). Given that the direction is a legal direction. Most easily applied using parent function findBricks and then specifying the direction.
RETURNS: List of bricks that a given move will influence, given the vertical direction. 
PRE:  movedirection is a legal direction (i.e. the direction inserted will result in a changed color)
      if movedirection is:  U (upwards) = 8
                            D (downwards) = -8
EXAMPLE:  findVert iniBoard (Just Black) 19  0 8 = [19,27]
          *iniBoard is the starting board of Othello/Reversi.
-}
          
findVert :: Board -> Maybe Reversi.Player -> Int -> Int -> Int -> [Int]

--VARIANT: number of rows.
findVert board player searchpoint acc int
  | searchpoint > 63 || searchpoint < 0 = []
  | findIndexColor board searchpoint == Nothing && acc /= 0 = []
  | acc <= 1 && findIndexColor board searchpoint == player = []
  | findIndexColor board searchpoint == player = [] 
  | otherwise = searchpoint : findVert board player (searchpoint+int) (acc+1) int

{- lastorfirst searchpoint int
Helper function to find out if, in findDiag or diagIsLegal is in last possible spot. More explicitly it returns the last or first number in the row, given the Int (Int is implicitly the direction)
RETURNS: Int objects representing the last or first number in the row. 
PRE:  int is -7,7,9,-9,1 or -1
EXAMPLE:  lastorfirst 5 (-9) = 0
          lastorfirst 5 (-7) = 7
          lastorfirst 44 (1) = 47
-}

lastorfirst :: Int -> Int -> Int
lastorfirst searchpoint int 
  | int == -7 || int == 9 || int == 1 = last (findRow searchpoint)
  | int == 7 || int == -9 || int == -1 = head (findRow searchpoint)


{- sortAndRemoveDupl x:xs
Performs quicksort on a list ascending order, but also removes duplicates. 
Version of quicksort where duplicates are removed, quicksort function was found and modified from https://wiki.haskell.org/Introduction#Quicksort_in_Haskell.
RETURNS: Sorted list without duplicates. 
EXAMPLES: sortAndRemoveDupl [1,2,4,3,2,3] = [1,2,3,4]
          sortAndRemoveDupl [1,2,3] = [1,2,3]
 -}
sortAndRemoveDupl :: Ord a => [a] -> [a]

--VARIANT: length (x:xs)
sortAndRemoveDupl []     = []
sortAndRemoveDupl (x:xs) = sortAndRemoveDupl less ++ [x] ++ sortAndRemoveDupl more
    where
        less  = filter (< x) xs
        more = filter (> x) xs
