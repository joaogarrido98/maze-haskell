module Main (get_maze, print_maze, is_wall, place_player, move, can_move, game_loop, get_path, main) where

import System.Environment

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
  let before = take pos list
      after = drop (pos + 1) list
   in before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char =
  let line = maze !! y
      new_line = modify_list line x char
      new_maze = modify_list maze y new_line
   in new_maze

--if path is empty return empty list
--else read file, unbox it and then return the unboxed with lines function
get_maze :: String -> IO [String]
get_maze [] = return []
get_maze path = do
  x <- readFile path
  let final = lines x
  return final

-- if maze is empty print nothing
-- else unline the maze and use print
print_maze :: [String] -> IO ()
print_maze [] = putStrLn ""
print_maze maze = do
  let final = unlines maze
  putStrLn final

-- if maze is empty return an error because  is_wall [] (x,y) = False doesn't make sense here 
-- if get maze coordinates returns empty then true else false
is_wall :: [String] -> (Int, Int) -> Bool
is_wall [] (x, y) = error "Empty Maze"
is_wall maze (x, y)
  | returned == ' ' = False
  | returned == '#' = True
  where returned = get maze x y
        (z,v) = get_target_position maze
        
-- if maze is empty return an empty board
-- else unbox the set function with the new char in the (x,y) position
place_player :: [String] -> (Int, Int) -> [String]
place_player [] (x, y) = []
place_player maze (x, y) = do
  setting <- set maze x y '@'
  return setting

--if dir is anything but wasd returns the same place
-- else adds or subtracts from the location is on
move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) dir
  | dir == 'w' = (x, y -1)
  | dir == 'a' = (x -1, y)
  | dir == 's' = (x, y + 1)
  | dir == 'd' = (x + 1, y)
  | otherwise = (x, y)

-- get the place where user wants to move from move function then
-- with that value check if is wall if its wall return false else true
can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move [] (x, y) dir = error "Empty maze"
can_move maze (x, y) dir = if is_wall maze movement == True then False else True
  where
    movement = move (x, y) dir

--put everything together to be able to play the game
game_loop :: [String] -> (Int, Int) -> IO ()
game_loop [] (x, y) = putStrLn []
game_loop maze (x, y) = do
  print_maze (place_player maze (x, y))
  user_input <- getLine
  let dir = head user_input
  if can_move maze (x, y) dir == True
    then game_loop maze (move (x, y) dir)
    else game_loop maze (x, y)

-- call an auxiliary function
get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path maze start end = head (get_path_aux maze [start] end [])

--if 
get_path_aux :: [String] -> [(Int, Int)] -> (Int,Int) -> [(Int,Int)]-> [[(Int,Int)]]
get_path_aux maze list end visited
  | list == [] = []
  | x == end = [visited ++ [x]]
  | otherwise = get_path_aux maze (get_possible_move maze x visited) end (visited ++ [x]) ++ get_path_aux maze xs end visited
  where x = head list
        xs = tail list

--from w a s d filter the ways it can move
-- then maps all of returned values from the filter with move to get the actual tuple of the positions
get_possible_move :: [String] -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
get_possible_move maze start visited = filter (\x -> not (x `elem` visited)) maped
                                     where filtro = filter (\dir -> is_wall maze (move start dir) == False) moves
                                           maped = map (move start) filtro
                                           moves = ['w', 'a', 's', 'd']

--get arguments inputed in the cmd then get the head, read the file use the line function
--after that call the get_path function to get the possible path of the maze
-- then print the maze with the dots using get_dotted_maze function
main :: IO ()
main = do
       args <- getArgs
       let x = head args
       file <- readFile x
       let maze = lines file
           path = get_path maze (1,1) (get_target_position maze) 
       get_dotted_maze maze path

--aux functions
--if list is empty return empty list
--else put a "." in the place the (x,y) gives
place_dot :: [String] -> (Int, Int) -> [String]
place_dot [] (x, y) = []
place_dot maze (x, y) = set maze x y '.'

--function that puts dots in each position required on the path
placing_dots_together :: [String] -> [(Int, Int)] -> [[String]]
placing_dots_together maze [] = []
placing_dots_together maze (x : xs) = place_dot maze_up x : placing_dots_together maze_up xs
                          where maze_up = place_dot maze x

--print the maze with the value returned from placing_dots_together
--after reversing it and getting the head of it
get_dotted_maze :: [String] -> [(Int, Int)] -> IO ()
get_dotted_maze maze visited = print_maze(head (reverse (placing_dots_together maze visited)))

--get the length of the maze to find the target position
get_target_position :: Foldable t => [t a] -> (Int, Int)
get_target_position (x : xs) = (col - 2, row - 2)
                               where col = length x
                                     row = length (x : xs)
