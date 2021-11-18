module TypeLet.Plugin.Substitution (letsToSubst) where

import Data.Bifunctor

import qualified Data.Graph as G

import GhcPlugins

import TypeLet.Plugin.Constraints

-- | Construct idempotent substitution
--
-- TODO: Not entirely sure if this is correct, might be too simplistic and/or an
-- abuse of the ghc API; it is a /whole/ lot simpler than @niFixTCvSubst@, which
-- is disconcerning.  Hwever, it seems to work for the examples so far; perhaps
-- our use case is simpler? Needs more thought.
letsToSubst :: [CLet] -> TCvSubst
letsToSubst = uncurry zipTvSubst . unzip . go [] . inorder
  where
    go :: [(TyVar, Type)] -> [(TyVar, Type)] -> [(TyVar, Type)]
    go acc []         = acc
    go acc ((x, t):s) = go ((x, t) : map (second (subst1 x t)) acc) s

    subst1 :: TyVar -> Type -> Type -> Type
    subst1 x t = substTyWith [x] [t]

-- | Order the assignments
--
-- Suppose we have two assignments
--
-- > x := xT
-- > y := yT    where  x in (freevars yT)
--
-- Then the substitution should map @y@ to @(x := xT) yT@. We do this by
-- constructing the substitution in order, adding assignments one by one,
-- applying them to all assignments already in the accumulated substitution
-- as we go. In this example, this means adding @y := yT@ /first/, so that
-- we can apply @x := xT@ later (note that recursive definitions are
-- impossible in our use case).
--
-- In order to find the right order, we construct a graph of assignments. To
-- continue with our example, this graph will contain an edge
--
-- > (y := yT) -----> (x := xT)
--
-- The required assignment ordering is then obtained by topological sort.
inorder :: [CLet] -> [(TyVar, Type)]
inorder lets =
    [ (x, t)
    | (CLet _ x t, _, _) <- map nodeFromVertex $ G.topSort graph
    ]
  where
    graph          :: G.Graph
    nodeFromVertex :: G.Vertex -> (CLet, TyVar, [TyVar])
    _vertexFromKey :: TyVar -> Maybe G.Vertex
    (graph, nodeFromVertex, _vertexFromKey) = G.graphFromEdges [
        ( l
        , y
        , [ x
          | CLet _ x _ <- lets
          , x `elemVarSet` (tyCoVarsOfType yT)
          ]
        )
      | l@(CLet _ y yT) <- lets -- variables name match description above
      ]

