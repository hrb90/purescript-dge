module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Foreign.EasyFFI (unsafeForeignFunction)
import Graphics.D3.Base (D3, D3Eff)
import Graphics.D3.Layout.Base (links, nodes, size)
import Graphics.D3.Layout.Force (ForceLayout, charge, createDrag, drag, forceLayout, linkDistance, onDragStart, onTick, start)
import Graphics.D3.Selection (append, attr, attr', bindData, enter, onDoubleClick, rootSelect, selectAll)
import Graphics.D3.Util ((..), (...))
import Prelude hiding (append)

-- | This is a PureScript adaptation of the Sticky Force Layout example:
-- | http://bl.ocks.org/mbostock/3750558


type GraphData =
  { nodes :: Array Node
  , links :: Array Link
  }

type Node = { x :: Number, y :: Number }
type Link = { source :: Node, target :: Node }


main :: forall eff. Eff (d3 :: D3, console :: CONSOLE | eff) Unit
main = void $ layoutData graph
  where a = { x: 100.0, y: 100.0 }
        b = { x: 200.0, y: 150.0 }
        c = { x: 300.0, y: 50.0 }
        d = { x: 400.0, y: 350.0 }
        e = { x: 500.0, y: 200.0 }
        f = { x: 600.0, y: 300.0 }
        g = { x: 700.0, y: 250.0 }
        graph = 
          {
            nodes: [ a, b, c, d, e, f, g ],
            links: [ { source: a, target: b }
                   , { source: b, target: c }
                   , { source: c, target: d }
                   , { source: c, target: e }
                   , { source: e, target: f }
                   , { source: f, target: g }
                   , { source: d, target: g }
                   ]
          }

layoutData :: forall eff. GraphData -> Eff (d3 :: D3, console :: CONSOLE | eff) ForceLayout
layoutData graph = do
  let canvasWidth = 960.0
      canvasHeight = 500.0

  _ <- log "hapax"
  force <- forceLayout
    .. size { width: canvasWidth, height: canvasHeight }
    .. charge (-400.0)
    .. linkDistance 40.0
  drag <- force ... drag
    .. onDragStart dragStartHandler
  svg <- rootSelect "body"
    .. append "svg"
    .. attr "width" canvasWidth
    .. attr "height" canvasHeight
  _ <- force
    ... nodes graph.nodes
    .. links graph.links
    .. start
  link <- svg ... selectAll ".link"
      .. bindData graph.links
    .. enter .. append "line"
      .. attr "class" "link"
  node <- svg ... selectAll ".node"
      .. bindData graph.nodes
    .. enter .. append "circle"
      .. attr "class" "node"
      .. attr "r" 12.0
      .. onDoubleClick doubleClickHandler
      .. createDrag drag
  force ... onTick \_ -> do
    _ <- link
      ... attr' "x1" (\d -> d.source.x)
      .. attr' "y1" (\d -> d.source.y)
      .. attr' "x2" (\d -> d.target.x)
      .. attr' "y2" (\d -> d.target.y)
    node
      ... attr' "cx" _.x
      .. attr' "cy" _.y

dragStartHandler :: forall d. d -> D3Eff Unit
dragStartHandler = unsafeForeignFunction ["d"] "d3.select(this).classed('fixed', d.fixed = true);"

doubleClickHandler :: forall d. d -> D3Eff Unit
doubleClickHandler = unsafeForeignFunction ["d"] "d3.select(this).classed('fixed', d.fixed = false);"