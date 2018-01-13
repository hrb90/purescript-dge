module GraphEditor where
  
import Control.Monad.Eff.Console (log)
import Graphics.D3.Base (D3, D3Eff)
import Graphics.D3.Layout.Base (links, nodes, size)
import Graphics.D3.Layout.Force (ForceLayout, charge, createDrag, drag, forceLayout, linkDistance, onDragStart, onTick, start)
import Graphics.D3.Selection (append, attr, attr', bindData, enter, onDoubleClick, rootSelect, select, selectAll)
import Graphics.D3.Util ((..), (...))
import Math (sqrt)
import Prelude hiding (append)

main = do
  let width = 960.0
      height = 500.0
      a = { id: 0, reflexive: false }
      b = { id: 1, reflexive: true }
      c = { id: 2, reflexive: false }
      graph = {
                nodes: [a, b, c],
                links: [ { source: a, target: b, left: false, right: true }
                       , { source: b, target: c, left: false, right: true }
                       ]
              }
  svg <- rootSelect "body"
    .. append "svg"
    .. attr "oncontextmenu" "return false;"
    .. attr "width" width
    .. attr "height" height

  force <- forceLayout
    .. size { width, height }
    .. nodes graph.nodes
    .. links graph.links
    .. linkDistance 150.0
    .. charge (-500.0)

  _ <- svg
    ... append "svg:defs"
    .. append "svg:marker"
    .. attr "id" "end-arrow"
    .. attr "viewBox" "0 -5 10 10"
    .. attr "refX" 6.0
    .. attr "markerWidth" 3.0
    .. attr "markerHeight" 3.0
    .. attr "orient" "auto"
    .. append "svg:path"
    .. attr "d" "M0,-5L10,0L0,5"
    .. attr "fill" "#000"
  
  _ <- svg
    ... append "svg:defs"
    .. append "svg:marker"
    .. attr "id" "start-arrow"
    .. attr "viewBox" "0 -5 10 10"
    .. attr "refX" 4.0
    .. attr "markerWidth" 3.0
    .. attr "markerHeight" 3.0
    .. attr "orient" "auto"
    .. append "svg:path"
    .. attr "d" "M0,-5L10,0L0,5"
    .. attr "fill" "#000"

  dragLine <- svg
    ... append "svg:path"
    .. attr "class" "link dragline hidden"
    .. attr "d" "M0,0L0,0"

  path <- svg
    ... append "svg:g"
    .. selectAll "path"
  
  circle <- svg
    ... append "svg:g"
    .. selectAll "g"

  force
    ... onTick \_ -> do
      _ <- path ... attr' "d" pathTick
      circle ... attr' "transform" circleTick
  
pathTick d = "M" <> sourceX <> "," <> sourceY <> "L" <> targetX <> "," <> targetY
  where deltaX = d.target.x - d.source.x
        deltaY = d.target.y - d.source.y
        dist = sqrt (deltaX * deltaX + deltaY * deltaY)
        normX = deltaX / dist
        normY = deltaY / dist
        sourcePadding = if d.left then 17.0 else 12.0
        targetPadding = if d.right then 17.0 else 12.0
        sourceX = show $ d.source.x + sourcePadding * normX
        sourceY = show $ d.source.y + sourcePadding * normY
        targetX = show $ d.target.x - targetPadding * normX
        targetY = show $ d.target.y - targetPadding * normY

circleTick { x, y } = "translate(" <> show x <> "," <> show y <> ")"