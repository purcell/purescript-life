-- TODO: restrict exports
module Grid
       ( Grid
       , Coord(..)
       , make
       , flatten
       , getAt
       , setAt
       , mapAt
       , mapWithIndex
       , neighbours
       ) where

import Prelude
import Data.Array as Arr
import Data.Array ((..))
import Data.Maybe (Maybe(..), fromMaybe)

type Grid a =
  { items :: Array a, width :: Int, height :: Int }


type Coord = { x :: Int, y :: Int }


neighbours :: Coord -> Array Coord
neighbours c = do
  dx <- -1 .. 1
  dy <- -1 .. 1
  if dx == 0 && dy == 0
    then mempty
    else pure { x : c.x + dx, y : c.y + dy }


make :: forall a . Int -> Int -> a -> Grid a
make width height def =
  { items : Arr.replicate (width * height) def
  , width : width
  , height : height
  }


indexToCoord :: forall a . Grid a -> Int -> Coord
indexToCoord g i = { x: (i  `mod` g.width), y : (i `div` g.width) }


coordToIndex :: forall a . Grid a -> Coord -> Int
coordToIndex g c = c.x + c.y * g.width


flatten :: forall a b. (Coord -> a -> b) -> Grid a -> Array b
flatten f g =
  Arr.mapWithIndex (\i item -> f (indexToCoord g i) item) g.items


getAt :: forall a. Coord -> Grid a -> Maybe a
getAt c g =
  Arr.index g.items (coordToIndex g c)


setAt :: forall a. Coord -> a -> Grid a -> Grid a
setAt c a g =
  g { items = fromMaybe g.items items' }
  where items' = Arr.updateAt (coordToIndex g c) a g.items


mapAt :: forall a. Grid a -> Coord -> (a -> a) -> Grid a
mapAt g c f =
    case getAt c g of
        Just cell -> setAt c (f cell) g
        Nothing -> g


mapWithIndex :: forall a b. (Coord -> a -> b) -> Grid a -> Grid b
mapWithIndex f g =
  g { items = Arr.mapWithIndex (\i c -> f (indexToCoord g i) c) g.items }
