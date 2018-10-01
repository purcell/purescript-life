module Component where

import Prelude
import Data.Array as Arr
import Data.Maybe (Maybe(..))
import Grid (Grid, Coord)
import Grid as Grid
import Svg as Svg
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HHP

data Query a = ToggleAt Coord a
             | ToggleRunning a
             | NextGeneration a
             | Reset a

data Cell = Alive | Dead

derive instance eqCell :: Eq Cell

toggle :: Cell -> Cell
toggle Alive = Dead
toggle Dead = Alive

type State = { grid :: Grid Cell, running :: Boolean, generations :: Int }

component :: forall m. H.Component HH.HTML Query Unit Void m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { grid : Grid.make 50 50 Dead, running : false, generations : 0 }

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_
      [ HH.header_
         [ HH.button
           [ HE.onClick (HE.input_ Reset) ]
           [ HH.text "Reset"]
         , HH.button
           [ HE.onClick (HE.input_ ToggleRunning) ]
           [ HH.text
             if not state.running then "Start" else "Stop"
           ]
         , HH.button
           [ HE.onClick (HE.input_ NextGeneration) ]
           [ HH.text "Next generation"]
         ]
      -- FIXME
      , Svg.svg [ HHP.width (50 * cellSize)
                , HHP.height (50 * cellSize)
                , Svg.viewBox "0 0 500 500"
                ]
                (Grid.flatten cellToSvg state.grid)
      ]

  cellToSvg pos cell =
    Svg.rect [ Svg.x (pos.x * cellSize)
             , Svg.y (pos.y * cellSize)
             , HHP.width cellSize
             , HHP.height cellSize
             , Svg.fill (case cell of
                            Alive -> (Svg.ColourName "black")
                            Dead -> (Svg.ColourName "white"))
             , Svg.stroke (Svg.ColourName "#ddd")
             , HE.onClick (HE.input_ (ToggleAt pos))
             ]

  cellSize = 10

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    ToggleRunning next -> do
      _ <- H.modify (\state -> state { running = not state.running })
      pure next
    ToggleAt pos next -> do
      _ <- H.modify (\state -> state { grid = Grid.mapAt state.grid pos toggle })
      pure next
    NextGeneration next -> do
      _ <- H.modify nextGenerationState
      pure next
    Reset next -> do
      _ <- H.modify (const initialState)
      pure next



nextGenerationState :: State -> State
nextGenerationState state =
  state { grid = nextGrid
        , running = state.running && (nextGrid /= state.grid)
        , generations = state.generations + 1 }
  where nextGrid = Grid.mapWithIndex (nextGenerationAt state.grid) state.grid


nextGenerationAt :: Grid Cell -> Coord -> Cell -> Cell
nextGenerationAt g pos cur =
  let liveNeighbours = countLiveNeighbours g pos in
  case cur of
    Alive ->
      if liveNeighbours < 2 || liveNeighbours > 3 then Dead else Alive
    Dead ->
      if liveNeighbours == 3 then Alive else Dead


countLiveNeighbours :: Grid Cell -> Coord -> Int
countLiveNeighbours grid pos =
  Arr.length (Arr.filter (\pos' -> Grid.getAt pos' grid == Just Alive) (Grid.neighbours pos))
