import Signal
import Graphics.Input (clickable)
import Graphics.Element (..)
import Text (asText, centered, fromString)
import Array
import Array (..)
import List
import Maybe
import Maybe (..)
import Color (..)
import Random (..)

type UserClick = UserClick Int Int | RestartGame Int Int

clickChannel : Signal.Channel UserClick
clickChannel = Signal.channel (RestartGame 0 0) -- define the default parameters here

board_width = 16
board_height = 16
mines = 40
cellSize = 20

clickSignal : Signal UserClick
clickSignal = Signal.subscribe clickChannel

type CellStatus = Closed | Open
type CellContents = Neighbors Int | Mine
type alias CellState = (CellStatus, CellContents)
type alias GameBoard = Array (Array CellState)
type GameStatus = Playing | Lost | StartingGame
type alias GameState = (GameStatus, GameBoard, Seed)

dummyState = (Closed, Neighbors (-1))

getBoardWidth board = Array.length board

getBoardHeight board = withDefault 0 <| Maybe.map Array.length (Array.get 0 board)

initializeState : Int -> Int -> Seed -> GameState
initializeState w h seed = (StartingGame, Array.repeat w (Array.repeat h dummyState), seed)

main : Signal Element
main = clickSignal
  |> Signal.foldp updateState (initializeState board_width board_height (initialSeed 12345))
  |> Signal.map drawScene

drawScene : GameState -> Element
drawScene (status, board, _) = flow down [spacer 10 10, flow right [spacer 10 10, drawBoard board, spacer 30 30, 
  case status of
    Lost -> 
      clickable (Signal.send clickChannel (RestartGame board_width board_height))
        <| color grey
        <| centered <| fromString "You lost, click this to clear game"
    StartingGame -> centered <| fromString "Click to begin"
    Playing ->
      clickable (Signal.send clickChannel (RestartGame board_width board_height))
        <| color grey
        <| centered <| fromString "Restart game"
  ]
 ]

totalBoardWidth board = (cellSize+2)*(getBoardWidth board)
totalBoardHeight board = (cellSize+2)*(getBoardHeight board)

drawBoard : GameBoard -> Element

drawBoard board = color black <| container (totalBoardWidth board + 2) (totalBoardHeight board + 2) middle <| flow right <| List.map drawColumn <| toIndexedList board

drawColumn : (Int, Array CellState) -> Element
drawColumn (x, column) = flow down <| List.map (drawCell' x) <| toIndexedList column

drawCell' x (y, c) = color black 
  <| container (cellSize+2) (cellSize+2) middle
  <| clickable (Signal.send clickChannel (UserClick x y))
  <| drawCell c

drawCell : CellState -> Element
drawCell (status, contents) =
  let
    cellElement = case contents of
      Mine -> color red <| container cellSize cellSize middle <| centered <| fromString "!"
      Neighbors 0 -> color white <| spacer (cellSize+2) (cellSize+2)
      Neighbors n -> color white <| container cellSize cellSize middle <| asText n
  in
    case status of
      Open -> cellElement
      _ -> color grey <| spacer cellSize cellSize

getCell : Int -> Int -> GameBoard -> CellState
getCell x y board = withDefault dummyState <| Array.get x board `andThen` (\column -> Array.get y column)

numberOfMines : Int -> Int -> GameBoard -> Int
numberOfMines x y board = case getCell x y board of
  (_, Mine) -> 1
  _ -> 0

updateState : UserClick -> GameState -> GameState
updateState click oldState = case (click, oldState) of
  (RestartGame w h, (status, board, seed)) -> initializeState w h seed
  (UserClick x y, (status, board, seed)) ->
    case status of
      StartingGame -> 
        let
          (newRandomBoard, newSeed) = fillRandomBoard x y mines board seed
        in
          (Playing, revealNeighbors [(x,y)] newRandomBoard, newSeed)
      Playing ->
        case getCell x y board of
          (Open, _) -> (Playing, board, seed)
          (Closed, Mine) -> (Lost, revealAll board, seed)
          (Closed, Neighbors _) -> (Playing, revealNeighbors [(x,y)] board, seed)
      _ -> (status, board, seed)

revealNeighbors list board =
  case list of
  [] -> board
  (x,y)::rest -> case (getCell x y board) of
    (Open, _) -> revealNeighbors rest board
    (Closed, Mine) -> revealNeighbors rest board
    (Closed, Neighbors 0) -> let newList = List.append (neighbors x y) rest in revealNeighbors newList <| modifyCell x y (Open, Neighbors 0) board
    (Closed, Neighbors n) -> revealNeighbors rest <| modifyCell x y (Open, Neighbors n) board

neighbors x y = [ (x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), 
  (x-1,y+1), (x,y+1), (x+1,y+1) ]

fillNeighbors board =
  let
    computeNeighbors x y (status, contents) =
      let
        nMines (x, y) = numberOfMines x y board
        sumNeighbors = List.foldl1 (+) <| List.map nMines  <| neighbors x y
      in
        case contents of
          Mine -> (status, Mine)
          _ ->  (status, Neighbors sumNeighbors)
    fillNeighborsInColumn x column = indexedMap (computeNeighbors x) column
  in
    indexedMap fillNeighborsInColumn board

fillRandomBoard : Int -> Int -> Int -> GameBoard -> Seed -> (GameBoard, Seed)
fillRandomBoard x y n board seed =
  if n == 0 then (fillNeighbors board, seed)
  else
    let
      (mineX, seed1) = generate (int 0 (getBoardWidth board - 1)) seed
      (mineY, seed2) = generate (int 0 (getBoardHeight board - 1)) seed1
      (cellStatus, cellContents) = getCell mineX mineY board
    in
      if mineX == x || mineY == y
      then
        fillRandomBoard x y n board seed2
      else  
        case cellContents of
          Mine -> fillRandomBoard x y n board seed2
          _ -> fillRandomBoard x y (n-1) (modifyCell mineX mineY (cellStatus, Mine) board) seed2

modifyCell : Int -> Int -> CellState -> GameBoard -> GameBoard
modifyCell x y cellState board = withDefault board <| Array.get x board `andThen` (\column -> Just <| Array.set x (Array.set y cellState column) board)

revealAll = Array.map (Array.map revealCell)
revealCell (status, contents) = (Open, contents)
