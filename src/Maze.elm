module Maze exposing (Point, Path, Maze, maze, path, solve)
import Set exposing (Set)
import Dict exposing (Dict)
import Random
import Random.Set

type alias Point = (Int, Int)
type alias Path = (Point, Point)
type alias Maze = { size: Point, start: Point, end: Point, paths: List Path }
type alias DistanceMap = Dict Point Int

path : Point -> Point -> Path
path ((x1, y1) as p1) ((x2, y2) as p2) =
  if x1 < x2 || (x1 == x2 && y1 < y2) then (p1, p2) else (p2, p1)

isInBound : Point -> Point -> Bool
isInBound (width, height) (x, y) = x >= 0 && y >= 0 && x < width && y < height

adjacentPoints : Point -> Point -> List Point
adjacentPoints size (x, y) =
  List.filter (isInBound size) [(x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

adjacentPaths : Point -> Point -> List Path
adjacentPaths size pt = List.map (path pt) (adjacentPoints size pt)

connectedPoints : List Path -> Point -> List Point
connectedPoints paths pt = List.filterMap (\(p1, p2) ->
    if p1 == pt then
      Just p2
    else if p2 == pt then
      Just p1
    else
      Nothing
  ) paths

xor : Set comparable -> Set comparable -> Set comparable
xor s1 s2 = Set.diff (Set.union s1 s2) (Set.intersect s1 s2)

maze : Random.Seed -> Point -> Maze
maze seed ((width, height) as size) =
  let
    start = (0, 0)
    generate : List Path -> Random.Seed -> Set Point -> Set Path -> List Path
    generate paths seed1 visited candidates =
      case Random.step (Random.Set.sample candidates) seed1 of
        (Nothing, _) -> paths
        (Just ((p1, p2) as selected), seed2) ->
          let
            newPoint = if Set.member p1 visited then p2 else p1
            newVisited = Set.insert newPoint visited
            newCandidates = xor candidates (Set.fromList (adjacentPaths size newPoint))
          in
            generate (selected :: paths) seed2 newVisited newCandidates
  in
    { size = size
    , start = start
    , end = (width - 1, height - 1)
    , paths = (generate [] seed (Set.singleton start) (Set.fromList (adjacentPaths size start)))
    }

solve : Maze -> List Point
solve { start, end, paths } =
  let
    distanceMap : Point -> DistanceMap -> DistanceMap
    distanceMap pt map =
      let
        children : List Point
        children = connectedPoints paths pt

        update : Int -> DistanceMap
        update dist = List.foldl distanceMap (Dict.insert pt dist map) children

        updateIfModified : Int -> DistanceMap
        updateIfModified dist =
          case Dict.get pt map of
            Nothing -> update dist
            Just origDist ->
              if origDist == dist then
                map
              else
                update dist

      in
        case List.minimum (List.filterMap (\p -> Dict.get p map) children) of
          Nothing -> updateIfModified 0
          Just dist -> updateIfModified (dist + 1)

    walk : Point -> List Point -> DistanceMap -> List Point
    walk pt route map =
      let
        children : List Point
        children = connectedPoints paths pt

        childrenWithDist = List.filterMap (\p -> Dict.get p map |> Maybe.map (Tuple.pair p)) children

        nextPoint = List.sortBy Tuple.second childrenWithDist |> List.head |> Maybe.map Tuple.first
      in
        case nextPoint of
          Nothing -> route
          Just p -> if p == start then (p :: route) else walk p (p :: route) map
  in
    distanceMap start (Dict.empty) |> walk end [end]
