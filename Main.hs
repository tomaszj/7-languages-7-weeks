module Main where
  import Data.List

  ------------------------------
  -- MAZE!!!111oneone
  ------------------------------

  -- in order to use [x][y] subscripts, the maze is an array of vertical arrays. (0, 0) is in bottom left corner,
  -- (MAX_X, 0) is in bottom right corner.

  data Exit = North | East | South | West deriving (Eq, Show)
  data MazeNode = Node [Exit] deriving Show
  type Maze = [[ MazeNode ]]

  type Location = (Int, Int)

  -- Path is a history of steps in the maze, with location and exit taken for each step
  type Path = [(Location, Exit)]

  -- Some test mazes

  simpleMaze :: Maze
  simpleMaze = [
    [Node [North],       Node [North, East, South], Node [South]],
    [Node [North, East], Node [North, West, South], Node [South]],
    [Node [North, West], Node [North, South]      , Node [South]]
    ]

  biggerMaze :: Maze
  biggerMaze = [
    [Node [East],       Node [North, East], Node [North, South], Node [North, South], Node [East, South]],
    [Node [East, West], Node [North, East, West], Node [North, East, South],
       Node [East, South], Node [East, West]],
    [Node [East, West], Node [North, East, West], Node [East, South, West], Node [West], Node [East, West]],
    [Node [East, West, North], Node [South, West], Node [West], Node [North, East], Node [West, South]],
    [Node [West], Node [North], Node [North, South], Node [South, West, North], Node []]
    ]

  -- access the node
  getNodeAt :: (Int, Int) -> Maze -> MazeNode
  getNodeAt (x, y) maze =
    maze !! x !! y

  -- determine the new location, if you take an exit
  locationInDirection :: Location -> Exit -> Location
  locationInDirection (x, y) exit
    | exit == North = (x, y + 1)
    | exit == East  = (x + 1, y)
    | exit == South = (x, y - 1)
    | otherwise     = (x - 1, y)

  -- That's one small step for a man, one giant leap for mankind.
  advanceInDirection :: (Int, Int) -> Exit -> Path -> ((Int, Int), Path)
  advanceInDirection location exit path_so_far =
    (locationInDirection location exit, path_so_far ++ [(location, exit)])

  -- Determine which exits in the current location cannot be taken
  remove_visited_locations :: Path -> Location -> [Exit] -> [Exit]
  remove_visited_locations path_so_far location node_exits =
    let isLocationNotOnThePath location =
          all (\(previous_location, exit) -> previous_location /= location)
    in filter (\exit -> isLocationNotOnThePath (locationInDirection location exit) path_so_far) node_exits

  -- SOLVE DA THING
  -- v2 - already traversed locations are excluded
  solveMaze :: (Int, Int) -> Maze -> Path -> Maybe Path
  solveMaze location@(current_x, current_y) maze path_so_far =
    let max_x = length maze - 1
        max_y = length (head maze) - 1
    in
      if current_x == max_x && current_y == max_y
      then Just path_so_far
      else
        let current_node = getNodeAt location maze
            Node all_node_exits = current_node
            possible_exits = remove_visited_locations path_so_far location all_node_exits
        in
          try_to_find_route possible_exits
          where try_to_find_route [] = Nothing
                try_to_find_route (exit : rem_exits) =
                  let (new_coordinates, new_path) =
                        advanceInDirection (current_x, current_y) exit path_so_far
                      result = solveMaze new_coordinates maze new_path
                  in case result of
                    Just path -> Just path
                    Nothing -> try_to_find_route rem_exits

 -- usage:
 -- `solveMaze (0, 0) simpleMaze []`
 -- `solveMaze (0, 0) biggerMaze []`
