{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RSTT.Interpret where

import           Control.Applicative
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Maybe           (fromMaybe)

import           RSTT.Syntax.Abs

import qualified RSTT.Cube            as RSTT
import qualified RSTT.Tope            as RSTT
import qualified RSTT.Tope.Proof      as Prover

interpret :: Program -> IO ()
interpret (Program decls) =
  evalStateT (mapM_ interpretDecl decls) Prover.rulesLJE

interpretDecl :: Decl -> StateT Prover.Rules IO ()
interpretDecl = \case
  DeclCube (Label con) _points -> lift $
    putStrLn ("WARNING: cube declaration is ignored for cube " <> show con)
  DeclTopePrefix (Label con) _cubes rules -> do
    forM_ rules $ \rule@(TopeRule (RuleName name) _ _ _) -> do
      lift $ putStrLn ("INFO: Adding new rule for tope " <> con <> ": " <> name)
      modify (<> convertRule rule)
  DeclShape (Var shapeName) _shape -> lift $
    putStrLn ("WARNING: shape definition is ignored for shape " <> show shapeName)
  DeclCommandProve sequent -> do
    let maxDepth = 10
    rules <- get
    lift $ do
      putStrLn ("INFO: running BFS prover with max depth = " <> show maxDepth)
      Prover.proveAndPrintBFS maxDepth rules (convertSequent sequent)
      putStrLn ""

-- ** Compiling rules

convertRule :: TopeRule -> Prover.Rules
convertRule (TopeRule (RuleName name) premises _line conclusion) = do
  currentGoal <- ask
  substs <- matchSequent conclusion currentGoal
  let goals = map (normalizeSequent . applySubsts substs) premises
  guard (all (/= currentGoal) goals)
  return (name, goals)

data Substs = Substs
  { substCubeVars  :: [(RSTT.Var, RSTT.Cube)]
  , substPointVars :: [(RSTT.Var, RSTT.Point)]
  , substTopeVars  :: [(RSTT.Var, RSTT.Tope)]
  }

matchSequent :: (MonadPlus f, MonadFail f) => Sequent -> Prover.Sequent -> f Substs
matchSequent (Sequent _points topes rhs) (Prover.Sequent _points' topes' rhs') = do
  subst <- matchTope rhs rhs'
  case topes of
    TopeContextEmpty -> return subst
    TopeContextNonEmpty topesList -> do
      matchTopes subst topesList topes'

matchTope :: MonadPlus f => Tope -> RSTT.Tope -> f Substs
matchTope (TopeVar (Var x)) tope = pure $ Substs [] [] [(RSTT.Var x, tope)]
matchTope (TopeCon (Label con) points) (RSTT.TopeCon (RSTT.Label con') points')
  | con /= con' = empty
  | length points /= length points' = empty
  | otherwise = zipWithM matchPoint points points' >>= mergeManySubsts
matchTope TopeTop RSTT.TopeTop = pure (Substs [] [] [])
matchTope TopeBottom RSTT.TopeBottom = pure (Substs [] [] [])
matchTope (TopeImplies x y) (RSTT.TopeImplies x' y') = do
  s1 <- matchTope x x'
  s2 <- matchTope y y'
  mergeSubsts s1 s2
matchTope (TopeOr x y) (RSTT.TopeOr x' y') = do
  s1 <- matchTope x x'
  s2 <- matchTope y y'
  mergeSubsts s1 s2
matchTope (TopeAnd x y) (RSTT.TopeAnd x' y') = do
  s1 <- matchTope x x'
  s2 <- matchTope y y'
  mergeSubsts s1 s2
matchTope (TopeEQ t s) (RSTT.TopeEQ t' s') = do
  s1 <- matchPoint t t'
  s2 <- matchPoint s s'
  mergeSubsts s1 s2
matchTope _ _ = empty

matchPoint :: MonadPlus f => Point -> RSTT.Point -> f Substs
matchPoint (PointVar (Var x)) point = pure $ Substs [] [(RSTT.Var x, point)] []
matchPoint (PointCon (Label con) points) (RSTT.PointCon (RSTT.Label con') points')
  | con /= con' = empty
  | length points /= length points' = empty
  | otherwise = zipWithM matchPoint points points' >>= mergeManySubsts
matchPoint PointUnit RSTT.PointUnit = pure (Substs [] [] [])
matchPoint (PointPair x y) (RSTT.PointPair x' y') = do
  s1 <- matchPoint x x'
  s2 <- matchPoint y y'
  mergeSubsts s1 s2
matchPoint (PointFirst x) (RSTT.PointFirst x') = matchPoint x x'
matchPoint (PointSecond x) (RSTT.PointSecond x') = matchPoint x x'
matchPoint _ _ = empty

matchTopes :: (MonadPlus f, MonadFail f) => Substs -> [Tope] -> [RSTT.Tope] -> f Substs
matchTopes substs [] _ = pure substs
matchTopes substs (tope:topes) topes' = do
  (tope'', topes'') <- Prover.selectOne topes'
  substs' <- matchTope tope tope''
  substs'' <- mergeSubsts substs substs'
  matchTopes substs'' topes topes''

mergeSubsts :: MonadPlus f => Substs -> Substs -> f Substs
mergeSubsts (Substs cubes points topes) (Substs cubes' points' topes') = Substs
  <$> merge cubes cubes'
  <*> merge points points'
  <*> merge topes topes'

mergeManySubsts :: MonadPlus f => [Substs] -> f Substs
mergeManySubsts = foldM mergeSubsts (Substs [] [] [])

merge
  :: (MonadPlus f, Eq k, Eq v, Show k, Show v)
  => [(k, v)] -> [(k, v)] -> f [(k, v)]
merge xs ys
  | conflicts = empty
  | otherwise = return (xs ++ ys)
  where
    conflicts = any conflict xs
    conflict (k, v) =
      case lookup k ys of
        Nothing -> False
        Just v' -> v /= v'

normalizeSequent :: Prover.Sequent -> Prover.Sequent
normalizeSequent = id -- FIXME

applySubsts :: Substs -> Sequent -> Prover.Sequent
applySubsts (Substs cubes points topes) (Sequent cubeContext topeContext tope) =
  Prover.Sequent
    (substInCubeContext cubes cubeContext)
    (substInTopeContext points topes topeContext)
    (substInTope points topes tope)

substInCubeContext :: [(RSTT.Var, RSTT.Cube)] -> CubeContext -> RSTT.CubeContext
substInCubeContext _ = convertCubeContext -- FIXME

substInTopeContext :: [(RSTT.Var, RSTT.Point)] -> [(RSTT.Var, RSTT.Tope)] -> TopeContext -> RSTT.TopeContext
substInTopeContext points topes = \case
  TopeContextEmpty       -> []
  TopeContextNonEmpty ts -> map (substInTope points topes) ts

substInTope :: [(RSTT.Var, RSTT.Point)] -> [(RSTT.Var, RSTT.Tope)] -> Tope -> RSTT.Tope
substInTope points topes = go
  where
    go = \case
      TopeTop         -> RSTT.TopeTop
      TopeBottom      -> RSTT.TopeBottom
      TopeVar (Var x) -> lookupTope x
      TopeCon (Label con) args -> RSTT.TopeCon (RSTT.Label con) $
        map (substInPoint points) args
      TopeImplies x y -> RSTT.TopeImplies (go x) (go y)
      TopeOr x y      -> RSTT.TopeOr (go x) (go y)
      TopeAnd x y     -> RSTT.TopeAnd (go x) (go y)
      TopeEQ x y     -> RSTT.TopeEQ (substInPoint points x) (substInPoint points y)

    lookupTope x = fromMaybe (RSTT.TopeVar (RSTT.Var x)) $
      lookup (RSTT.Var x) topes

substInPoint :: [(RSTT.Var, RSTT.Point)] -> Point -> RSTT.Point
substInPoint points = go
  where
    go = \case
      PointUnit        -> RSTT.PointUnit
      PointVar (Var x) -> lookupPoint x
      PointCon (Label con) args -> RSTT.PointCon (RSTT.Label con) $
        map go args
      PointPair x y    -> RSTT.PointPair (go x) (go y)
      PointFirst x     -> RSTT.PointFirst (go x)
      PointSecond x    -> RSTT.PointSecond (go x)

    lookupPoint x = fromMaybe (RSTT.PointVar (RSTT.Var x)) $
      lookup (RSTT.Var x) points

-- ** Converting syntax

convertSequent :: Sequent -> Prover.Sequent
convertSequent (Sequent points topes tope) = Prover.Sequent
  (convertCubeContext points)
  (convertTopeContext topes)
  (convertTope tope)

convertCubeContext :: CubeContext -> RSTT.CubeContext
convertCubeContext CubeContextEmpty = []
convertCubeContext (CubeContextNonEmpty pairs) = map convert pairs
  where
    convert (PointDecl (Var t) cube) = (RSTT.Var t, convertCube cube)

convertCube :: Cube -> RSTT.Cube
convertCube CubeUnit              = RSTT.CubeUnit
convertCube (CubeProduct i j)     = RSTT.CubeProduct (convertCube i) (convertCube j)
convertCube (CubeCon (Label con)) = RSTT.CubeCon (RSTT.Label con)
convertCube (CubeVar (Var i))     = RSTT.CubeVar (RSTT.Var i)

convertPoint :: Point -> RSTT.Point
convertPoint PointUnit       = RSTT.PointUnit
convertPoint (PointPair t s) = RSTT.PointPair (convertPoint t) (convertPoint s)
convertPoint (PointFirst t)  = RSTT.PointFirst (convertPoint t)
convertPoint (PointSecond t) = RSTT.PointSecond (convertPoint t)
convertPoint (PointCon (Label con) points)
  = RSTT.PointCon (RSTT.Label con) (map convertPoint points)
convertPoint (PointVar (Var i))     = RSTT.PointVar (RSTT.Var i)

convertTope :: Tope -> RSTT.Tope
convertTope TopeTop           = RSTT.TopeTop
convertTope TopeBottom        = RSTT.TopeBottom
convertTope (TopeImplies x y) = RSTT.TopeImplies (convertTope x) (convertTope y)
convertTope (TopeOr x y)      = RSTT.TopeOr (convertTope x) (convertTope y)
convertTope (TopeAnd x y)     = RSTT.TopeAnd (convertTope x) (convertTope y)
convertTope (TopeEQ t s)      = RSTT.TopeEQ (convertPoint t) (convertPoint s)
convertTope (TopeCon (Label con) points)
  = RSTT.TopeCon (RSTT.Label con) (map convertPoint points)
convertTope (TopeVar (Var phi)) = RSTT.TopeVar (RSTT.Var phi)

convertTopeContext :: TopeContext -> RSTT.TopeContext
convertTopeContext TopeContextEmpty            = []
convertTopeContext (TopeContextNonEmpty topes) = map convertTope topes

