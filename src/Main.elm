module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, style, type_, value)
import Maze exposing (Maze, Path, Point)
import Random
import Set

type alias Model =
  { width : String
  , height : String
  , maze : Maybe Maze
  , solution: List Point
  }

init : (Model, Cmd Msg)
init = ({ width = "", height = "", maze = Nothing, solution = [] }, Cmd.none)

type Msg = ChangeWidth String | ChangeHeight String | Generate | GenerateResult Maze | Solve

isSizeValid : Point -> Bool
isSizeValid (width, height) = width >= 2 && height >= 2 && width <= 60 && height <= 60

mazeGenerator : Point -> Random.Generator Maze
mazeGenerator size = Random.map (Maze.maze size) Random.independentSeed

alternate : (a -> c) -> (b -> c) -> List a -> List b -> List c
alternate f1 f2 l1 l2 =
  case l1 of
    x::xs ->
      case l2 of
        y::ys -> ((f1 x) :: (f2 y) :: (alternate f1 f2 xs ys))
        [] -> [f1 x]
    [] -> []

renderMaze : Maze -> List Point -> Html Msg
renderMaze { size, start, end, paths } sol =
  let
    (width, height) = size

    black = style "background-color" "black"
    gray = style "background-color" "gray"
    red = style "background-color" "red"
    white = style "background-color" "white"

    pathSet = Set.fromList paths
    solutionSet = Set.fromList sol
    solutionPaths = List.map2 Maze.path sol (List.drop 1 sol)
    solutionPathSet = Set.fromList solutionPaths

    renderPoint : Point -> Html Msg
    renderPoint pt = div [
        if (pt == start || pt == end) then
          red
        else if Set.member pt solutionSet then
          gray
        else
          white
      ] []

    renderPath : Path -> Html Msg
    renderPath ph = div [
        if Set.member ph solutionPathSet then
          gray
        else if Set.member ph pathSet then
          white
        else
          black
      ] []

    renderCorner : Html Msg
    renderCorner = div [black] []

    pointsOnRow : Int -> List Point
    pointsOnRow row = List.map2 Tuple.pair (List.range 0 (width - 1)) (List.repeat width row)

    renderPointRow : Int -> List (Html Msg)
    renderPointRow row =
      let
        points : List Point
        points = pointsOnRow row

        edges : List Path
        edges = List.map2 Tuple.pair points (List.drop 1 points)
      in
        alternate renderPoint renderPath points edges

    renderPathRow : Int -> List (Html Msg)
    renderPathRow row =
      let
        points : List Point
        points = pointsOnRow row

        pointsBelow : List Point
        pointsBelow = pointsOnRow (row + 1)

        edges : List Path
        edges = List.map2 Tuple.pair points pointsBelow
      in
        alternate renderPath (\_ -> renderCorner) edges (List.repeat (width - 1) ())

    pointSize = "1fr"
    pathSize = (String.fromInt (200 // width)) ++ "px"

    gridTemplate : Int -> String
    gridTemplate length = String.join " " (alternate (\_ -> pointSize) (\_ -> pathSize) (List.repeat length ()) (List.repeat (length - 1) ()))

    addBorder : String -> String
    addBorder s = pathSize ++ " " ++ s ++ " " ++ pathSize
  in
    div
      [ style "display" "grid"
      , style "grid-template-rows" (gridTemplate height |> addBorder)
      , style "grid-template-columns" (gridTemplate width |> addBorder)
      , style "aspect-ratio" (String.fromFloat ((toFloat width) / (toFloat height)))
      ]
      (List.append
        (List.concat (alternate renderPointRow renderPathRow (List.range 0 (height - 1)) (List.range 0 (height - 2))))
        [ div [style "grid-area" "1 / 1 / 1 / -1", black] []
        , div [style "grid-area" "-2 / 1 / -2 / -1", black] []
        , div [style "grid-area" "2 / 1 / -2 / 1", black] []
        , div [style "grid-area" "2 / -2 / -2 / -2", black] []
        ]
      )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeWidth w -> ({ model | width = w }, Cmd.none)
    ChangeHeight h -> ({ model | height = h }, Cmd.none)
    Generate ->
      case Maybe.map2 Tuple.pair (String.toInt model.width) (String.toInt model.height) of
        Nothing -> (model, Cmd.none)
        Just size ->
          if isSizeValid size then
            (model, Random.generate GenerateResult (mazeGenerator size))
          else
            ({ model | maze = Nothing, solution = [] }, Cmd.none)
    GenerateResult maze -> ({ model | maze = Just maze, solution = [] }, Cmd.none)
    Solve -> ({ model | solution = Maybe.map Maze.solve model.maze |> Maybe.withDefault [] }, Cmd.none)

view : Model -> Html Msg
view model =
  div [style "padding" "20px"]
    [ div [style "padding-bottom" "20px"]
      [ text "Min size: 2. Max size: 60"
      ]
    , div [style "padding-bottom" "20px"]
      [ text "Width"
      , input [ placeholder "width", type_ "number", value model.width, onInput ChangeWidth ] []
      , text "Height"
      , input [ placeholder "height", type_ "number", value model.height, onInput ChangeHeight ] []
      , button [ onClick Generate ] [ text "Generate" ]
      , button [ onClick Solve ] [ text "Solve" ]
      ]
    , div [style "max-width" "800px"]
      (case model.maze of
        Nothing -> []
        Just maze -> [renderMaze maze model.solution]
      )
    ]

main : Program () Model Msg
main =
  Browser.element { init = \_ -> init, update = update, view = view, subscriptions = \_ -> Sub.none }
