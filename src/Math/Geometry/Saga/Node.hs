module Math.Geometry.Saga.Node (getAllRoutes)
where
import qualified Data.Map as M
import Math.Geometry.Saga.Types

-- | find all possible edges between Nodes
findEdges :: NodeMap -> [((String,String),[String])]
findEdges nds =
  [((fromNme,toNme),[o]) | (fromNme, (_, fromOuts)) <- M.toList nds
                       , (toNme, (toIns, _)) <- M.toList nds
                       , o <- fromOuts
                       , i <- toIns
                       , o == i
                       ]


-- | Get all possible routes for some nodes
getAllRoutes :: NodeMap -> [((String,String),[String])]
getAllRoutes = concat . findRoutes . findEdges

-- | finds all possible routes
findRoutes :: [((String,String),[String])] -> [[((String,String),[String])]]
findRoutes [] = []
findRoutes edgs = edgs : edgs' : findRoutes edgs'
  where
    edgs' = chainEdges edgs
    chainEdges :: [((String,String),[String])] -> [((String,String),[String])]
    chainEdges edges =
      [((srcF,dstT), srcCmds ++ dstCmds) | ((srcF,srcT),srcCmds) <- edges
                                         , ((dstF,dstT),dstCmds) <- edges
                                         , srcT == dstF
                                         ]
