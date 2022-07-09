module Maze exposing (Point, Path, Maze, newPath, solve, generate)
import Set exposing (Set)
import Dict exposing (Dict)
import Random
import Random.Set

type alias Point = (Int, Int)
type alias Path = (Point, Point)
type alias Maze = { size: Point, start: Point, end: Point, paths: List Path }
type alias DistanceMap = Dict Point Int
type alias Walk = { at: Point, prev: Point, route: List Point, points: Set Point }

newPath : Point -> Point -> Path
newPath ((x1, y1) as p1) ((x2, y2) as p2) =
  if x1 < x2 || (x1 == x2 && y1 < y2) then (p1, p2) else (p2, p1)

isInBound : Point -> Point -> Bool
isInBound (width, height) (x, y) = x >= 0 && y >= 0 && x < width && y < height

adjacentPoints : Point -> Point -> List Point
adjacentPoints size (x, y) =
  List.filter (isInBound size) [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

dropWhile : (a -> Bool) -> List a -> List a
dropWhile f list =
  case list of
    (x::xs) -> if f x then dropWhile f xs else list
    [] -> []

newWalk : Point -> Walk
newWalk pt = { at = pt, prev = pt, route = [pt], points = Set.singleton pt }

walkTo : Point -> Walk -> Walk
walkTo pt { at, route, points } =
  if Set.member pt points then
    let newRoute = dropWhile (\p -> p /= pt) route
    in  { at = pt, prev = at, route = newRoute, points = Set.fromList newRoute }
  else
    { at = pt, prev = at, route = (pt :: route), points = Set.insert pt points }

walk : Point -> Set Point -> Random.Seed -> Walk -> (List Point, Random.Seed)
walk size visited seed ({ at, prev, route } as currentWalk) =
  case Random.step (adjacentPoints size at |> Set.fromList |> Set.remove prev |> Random.Set.sample) seed of
    (Nothing, newSeed) -> ([], newSeed)
    (Just pt, newSeed) ->
      if Set.member pt visited then (pt::route, newSeed)
      else walk size visited newSeed (walkTo pt currentWalk)

generateMaze : Set Point -> Set Point -> Random.Seed -> Maze -> Maze
generateMaze visited unvisited seed maze =
  case Random.step (Random.Set.sample unvisited) seed of
    (Nothing, _) -> maze
    (Just pt, seed1) ->
      let
        (route, seed2) = walk maze.size visited seed1 (newWalk pt)
        newlyVisited = Set.fromList route
        newVisited = Set.union visited newlyVisited
        newUnvisited = Set.diff unvisited newlyVisited
        paths = List.map2 newPath route (List.drop 1 route)
        newMaze = { maze | paths = List.append paths maze.paths }
      in generateMaze newVisited newUnvisited seed2 newMaze

allPoints : Point -> List Point
allPoints (width, height) = List.concatMap (\x -> List.map (Tuple.pair x) (List.range 0 (height - 1))) (List.range 0 (width - 1))

generate : Point -> Random.Seed -> Maze
generate ((width, height) as size) seed =
  let
    initialPoint = (0, 0)
    visited = Set.singleton initialPoint
    unvisited = allPoints size |> Set.fromList |> Set.remove initialPoint
    maze = { size = size, start = initialPoint, end = (width - 1, height - 1), paths = [] }
  in generateMaze visited unvisited seed maze

solve : Maze -> List Point
solve { start, end, paths } =
  let
    addToConn : Point -> Point -> Dict Point (List Point) -> Dict Point (List Point)
    addToConn k v dict = Dict.insert k (v::(Dict.get k dict |> Maybe.withDefault [])) dict

    connectionMap : Dict Point (List Point)
    connectionMap = List.foldl (\(p1, p2) dict -> addToConn p1 p2 dict |> addToConn p2 p1) Dict.empty paths

    distanceMap : Point -> DistanceMap -> DistanceMap
    distanceMap pt map =
      let
        children : List Point
        children = Dict.get pt connectionMap |> Maybe.withDefault []

        update : Int -> DistanceMap
        update dist = List.foldl distanceMap (Dict.insert pt dist map) children

        updateIfSmaller : Int -> DistanceMap
        updateIfSmaller dist =
          case Dict.get pt map of
            Nothing -> update dist
            Just origDist ->
              if origDist <= dist then
                map
              else
                update dist

      in
        case List.minimum (List.filterMap (\p -> Dict.get p map) children) of
          Nothing -> updateIfSmaller 0
          Just dist -> updateIfSmaller (dist + 1)

    getRoute : Point -> List Point -> DistanceMap -> List Point
    getRoute pt route map =
      let
        children : List Point
        children = Dict.get pt connectionMap |> Maybe.withDefault []

        childrenWithDist = List.filterMap (\p -> Dict.get p map |> Maybe.map (Tuple.pair p)) children

        nextPoint = List.sortBy Tuple.second childrenWithDist |> List.head |> Maybe.map Tuple.first
      in
        case nextPoint of
          Nothing -> route
          Just p -> if p == start then (p :: route) else getRoute p (p :: route) map
  in
    distanceMap start (Dict.empty) |> getRoute end [end]
