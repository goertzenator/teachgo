{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

import Reflex
import Reflex.Dom
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Text (pack, unpack, Text)
import Text.Read (readMaybe)
import Control.Applicative ((<*>), (<$>))
import Data.Monoid ((<>))

data BoardSize = Size9 | Size13 | Size19 deriving (Ord, Eq)

boardDimension :: BoardSize -> Int
boardDimension Size9 = 9
boardDimension Size13 = 13
boardDimension Size19 = 19

starPoints :: BoardSize -> [(Int, Int)]
starPoints Size9 = [(2,2), (6,2), (4,4), (2,6), (6,6)]
starPoints Size13 = [(3,3), (9,3), (6,6), (3,9), (9,9)]
starPoints Size19 = [ (x,y) | let s=[3,9,15], x<-s, y<-s]


data StoneColor = Black | White deriving (Eq, Show)

dynStone :: MonadWidget t m => Int -> Int -> Dynamic t (Maybe StoneColor) -> m(Event t ())
dynStone x y dynMaybeStoneColor = do
  (c, _) <- svgDynAttr' "circle" (stoneAttrs x y <$> dynMaybeStoneColor) blank
  return $ (\_ -> ()) <$> (domEvent Click c)

stoneAttrs x y maybeStoneColor =
  let fill = if maybeStoneColor == Just Black then "black" else "white"
      opacity = if isJust maybeStoneColor then "0.95" else "0.05"
  in
    Map.fromList [
        ("cx", pack $ show x),
        ("cy", pack $ show y),
        ("r", "0.43"),
        ("fill", fill),
        ("opacity", opacity),
        ("style", "stroke:black;stroke-width:0.01")]

-- Stone that toggles when clicked.  Color is determined by behavior parameter
toggleStone :: MonadWidget t m => Int -> Int -> Behavior t StoneColor -> m()
toggleStone x y behaviorMaybeColor = do
  currentColor <- sample behaviorMaybeColor
  let mcolor False = Nothing
      mcolor True  = Just currentColor
  rec
    visible <- toggle False clickEv
    clickEv <- dynStone x y (mcolor <$> visible) -- returns nested Event, how to resolve?
  blank

-- Always-on stone with selection border 
paletteStone :: MonadWidget t m => Int -> Int -> StoneColor -> Dynamic t Bool-> m(Event t())
paletteStone x y color selected = do
  let margin = 0.05
      start = -0.5 + margin
      offset = 1.0 - (2.0*margin)
      fx = fromIntegral x
      fy = fromIntegral y
      attrs select = Map.fromList [
        ("x", (pack.show) $ fx+start),
        ("y", (pack.show) $ fy+start),
        ("width", (pack.show) offset),
        ("height", (pack.show) offset),
        ("fill", "none"),
        ("opacity", if select then "0.95" else "0.05"),
        ("style", "stroke:black;stroke-width:0.01;stroke-dasharray: 0.02 0.02;")]
  svgDynAttr "rect" (attrs <$> selected) blank
  dynStone x y (constDyn (Just color))


-- Palette containing a stone of each color
paletteWidget :: MonadWidget t m => m(Behavior t StoneColor)
paletteWidget =
  svg "g" $ do
    rec
      -- Combine Event t () from both palette stones into single Event t StoneColor
      -- Keep the first event if more than one happen simultaneously
      let pickColor = mergeWith const [
              const Black <$> pickBlack,
              const White <$> pickWhite ]

        -- And store current color as a Dynamic
      selectedColor <- holdDyn Black pickColor

      -- The palette stones with selection box logic
      pickBlack <- paletteStone (-1) 0 Black $ fmap (==Black) selectedColor
      pickWhite <- paletteStone (-1) 1 White $ fmap (==White) selectedColor
    return $ current selectedColor


-- boardWidget :: MonadWidget t m => BoardSize -> m (Event t (Int,Int))
boardWidget :: MonadWidget t m => BoardSize -> Behavior t StoneColor -> m ()
boardWidget bs bcurrentColor = do
  let
    dim = (boardDimension bs) - 1
    dimtxt = show $ (fromIntegral dim) + 1.0
    viewbox = pack $ "-0.5 -0.5 " ++ dimtxt ++ " " ++ dimtxt

    svgLine x1 y1 x2 y2 = svgAttr "line" (Map.fromList [
        ("x1", pack $ show x1),
        ("y1", pack $ show y1),
        ("x2", pack $ show x2),
        ("y2", pack $ show y2),
        ("style", "stroke:black;stroke-width:0.02;stroke-linecap:square")])
      blank

    svgStar x y = svgAttr "circle" (Map.fromList [
        ("cx", pack $ show x),
        ("cy", pack $ show y),
        ("r", "0.05"),
        ("style", "fill:black;stroke:black;stroke-width:0.001")])
      blank

  svg "g" $ do
      sequence_ [ svgLine x 0 x dim | x <- [0..dim] ]   -- vertical lines
      sequence_ [ svgLine 0 y dim y | y <- [0..dim] ]   -- horizontal lines
      sequence_ [ svgStar x y | (x,y) <- starPoints bs] -- star points
      sequence_ [ toggleStone x y bcurrentColor | x <- [0..dim], y <- [0..dim]] -- stones


bodyElement :: MonadWidget t m => m ()
bodyElement = do
  -- sizepicker <- dropdown Size9 (constDyn boardPickerMap) def
  -- el "p" blank

  let
    boardSize = Size9
    dim = (fromIntegral.boardDimension) boardSize

    -- View for whole board plus palette along left edge
    viewbox = pack $ "-1.5 -0.5 " ++ (show $ dim + 1.0) ++ " " ++ (show dim)

  svgAttr "svg" (Map.fromList [
    ("viewBox", viewbox)
    ]) $ do

    currentColor <- paletteWidget
    -- svgAttr "line" (Map.FromList)
    boardWidget boardSize currentColor

  blank



headElement :: MonadWidget t m => m ()
headElement = do
  el "title" $ text "Teach Go"
  el "style" $ text styleSheet
  where
    styleSheet = "html, body { margin:0; padding:0; overflow:hidden; background-color: WhiteSmoke }; svg { position:fixed; top:0; left:0; height:100%; width:100% }"


main :: IO ()
main = mainWidgetWithHead headElement bodyElement


-- boardPickerMap :: Map.Map BoardSize Text
-- boardPickerMap = Map.fromList [
--   (Size9, "9x9"),
--   (Size13, "13x13"),
--   (Size19, "19x19")]




-- Lifted from Reflex.Dom.Contrib.Widgets.Svg
-- TODO: figure out how to use this module directly
{-# INLINABLE svgDynAttr' #-}
svgDynAttr' :: forall t m a. MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m (El t, a)
svgDynAttr' = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

{-# INLINABLE svgDynAttr #-}
svgDynAttr :: forall t m a. MonadWidget t m => Text -> Dynamic t (Map Text Text) -> m a -> m a
svgDynAttr elementTag attrs child = snd <$> svgDynAttr' elementTag attrs child

{-# INLINABLE svgAttr' #-}
svgAttr' :: forall t m a. MonadWidget t m => Text -> Map Text Text -> m a -> m (El t, a)
svgAttr' elementTag attrs child = svgDynAttr' elementTag (constDyn attrs) child

{-# INLINABLE svgAttr #-}
svgAttr :: forall t m a. MonadWidget t m => Text -> Map Text Text -> m a -> m a
svgAttr elementTag attrs child = svgDynAttr elementTag (constDyn attrs) child

{-# INLINABLE svg' #-}
svg' :: forall t m a. MonadWidget t m => Text -> m a -> m (El t, a)
svg' elementTag child = svgAttr' elementTag (Map.empty :: AttributeMap) child

{-# INLINABLE svg #-}
svg :: forall t m a. MonadWidget t m => Text -> m a -> m a
svg elementTag child = svgAttr elementTag Map.empty child

svgClass :: forall t m a. MonadWidget t m => Text -> Text -> m a -> m a
svgClass elementTag c child = svgAttr elementTag ("class" =: c) child
