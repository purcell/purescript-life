module Svg
       ( svg
       , rect
       , ColourName(..)
       , x
       , y
       , width
       , height
       , stroke
       , fill
       , viewBox
       ) where


import Data.Int as Int
import Halogen.HTML as HH
import Halogen.HTML.Properties as HHP
import DOM.HTML.Indexed

-- Elements

type SVGsvg = Interactive (height :: CSSPixel, width :: CSSPixel, viewBox :: String)


svg :: forall p i. HH.Node SVGsvg p i
svg = HH.element (HH.ElemName "svg")


type SVGrect = Interactive (x :: CSSPixel, y :: CSSPixel, width :: CSSPixel, height :: CSSPixel, fill :: ColourName, stroke :: ColourName)


rect :: forall p i. HH.Leaf SVGrect p i
rect props = HH.element (HH.ElemName "rect") props []


-- Properties

newtype ColourName = ColourName String


viewBox :: forall r i. String -> HH.IProp (viewBox :: String | r) i
viewBox = HH.attr (HH.AttrName "viewBox")


width :: forall r i. Int -> HH.IProp (width :: Int | r) i
width = HHP.width


height :: forall r i. Int -> HH.IProp (height :: Int | r) i
height = HHP.height


x :: forall r i. Int -> HH.IProp (x :: Int | r) i
x v = HH.attr (HH.AttrName "x") (Int.toStringAs Int.decimal v)


y :: forall r i. Int -> HH.IProp (y :: Int | r) i
y v = HH.attr (HH.AttrName "y") (Int.toStringAs Int.decimal v)


stroke :: forall r i. ColourName -> HH.IProp (stroke :: ColourName | r) i
stroke (ColourName n) = HH.attr (HH.AttrName "stroke") n


fill :: forall r i. ColourName -> HH.IProp (fill :: ColourName | r) i
fill (ColourName n) = HH.attr (HH.AttrName "fill") n
