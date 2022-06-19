{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module RSTT.Interpret where

import           Data.Set             (Set)
import qualified Data.Set             as Set

import           Control.Applicative
import           Control.Monad.Logic
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
#if __GLASGOW_HASKELL__ < 808
import           Control.Monad.Fail   (MonadFail)
#endif
import           Data.Maybe           (fromMaybe)

import           RSTT.Syntax.Abs
import qualified RSTT.Syntax.Layout   as RSTT
import qualified RSTT.Syntax.Par      as RSTT

import qualified RSTT.Cube            as RSTT
import qualified RSTT.Tope            as RSTT
import qualified RSTT.Tope.Proof      as Prover

interpretIO :: String -> IO ()
interpretIO input = do
  case interpret input of
    Left err -> do
      putStrLn "ERROR: Syntax error:"
      putStrLn err
    Right outputs -> mapM_ putStrLn outputs

interpretProgramIO :: Program -> IO ()
interpretProgramIO = mapM_ putStrLn . interpretProgram

interpret :: String -> Either String [String]
interpret input = do
  let tokens = RSTT.resolveLayout True (RSTT.myLexer input)
  interpretProgram <$> RSTT.pProgram tokens

interpretProgram :: Program -> [String]
interpretProgram (Program decls) =
  execWriter $
    evalStateT (mapM_ interpretDecl decls) Prover.rulesLJE

interpretDecl :: Decl -> StateT Prover.DefinedRules (Writer [String]) ()
interpretDecl = \case
  DeclCube (Label con) _points -> lift $
    tell [ "WARNING: cube declaration is ignored for cube " <> con ]
  DeclTopePrefix (Label con) _cubes rules -> do
    forM_ rules $ \rule@(TopeRule (RuleName name) _ _ _) -> do
      tell [ "INFO: Adding new rule for tope " <> con <> ": " <> name ]
      modify (<> convertRule rule)
  DeclShape (Var shapeName) _shape -> lift $
    tell [ "WARNING: shape definition is ignored for shape " <> shapeName ]
  DeclCommandProve sequent -> do
    let maxDepth = 20
        k = 1
    rules <- gets Prover.fromDefinedRules
    lift $ do
      case (maxDepth, k) of
        (_, 1) -> tell
          [ "INFO: running BFS proofsearch up to depth " <> show (maxDepth * k) ]
        (1, _) -> tell
          [ "INFO: running DFS proofsearch up to depth " <> show (maxDepth * k) ]
        _ -> tell
          [ "INFO: running k-depth proofsearch up to depth " <> show (maxDepth * k) <>
            " (" <> show maxDepth <> " iterations, k = " <> show k <> ")" ]
      let s = convertSequent sequent
      case Prover.proveWithBFSviaDFS' maxDepth k rules s of
        Nothing -> do
          tell [ "The sequent is not provable: " <> Prover.ppSequent s ]
        Just proof -> do
          tell [ Prover.ppProof proof ]
      tell [ "" ]

-- ** Compiling rules

convertRule :: TopeRule -> Prover.DefinedRules
convertRule rule = Prover.DefinedRules
  { invertibleRules    = tableauRule rule
  , invertibleCutRules = tableauCutRule rule
  , tableauxRules      = mempty -- tableauRule rule
  , tableauxCutRules   = mempty -- tableauCutRule rule
  }

tableauRule :: TopeRule -> Prover.Rules
tableauRule rule@(TopeRule (RuleName name) premises _line conclusion) = do
  currentGoal <- ask
  let metavars = foldMap collectPointVars (collectRulePoints rule)
  (substs, leftoverTopes) <- matchSequent metavars conclusion currentGoal
  let goals = map (normalizeSequent . addTopes leftoverTopes . applySubsts substs) premises
  guard (all (/= currentGoal) goals)
  -- trace (Prover.ppSequent currentGoal) $
  return (name, goals)
  where
    addTopes topes Prover.Sequent{..} =
      Prover.Sequent{ sequentTopeContext = topes <> sequentTopeContext, ..}

tableauCutRule :: TopeRule -> Prover.Rules
tableauCutRule rule@(TopeRule (RuleName name) premises _line conclusion) = do
  -- [Heuristic] affects completeness
  -- guard (null premises)

  currentGoal@Prover.Sequent{..} <- ask
  let metavars = foldMap collectPointVars (collectRulePoints rule)
  let Sequent _ ctx tope = conclusion

  -- [Heuristic] TODO: check if affects completeness
  -- do not consider rules where RHS is a variable
  guard (not (isVar tope))

  (substs@(Substs _ substPoints substTopes), leftoverTopes) <- matchTopeContext (Substs [] [] []) ctx sequentTopeContext
  subst''@(Substs _ substPoints' _) <- matchVars (metavars `Set.difference` Set.fromList (fst <$> substPoints))
              (foldMap collectTopePoints (sequentTope : sequentTopeContext))
  let substs' = substs <> subst''
      newTope = substInTope (substPoints <> substPoints') substTopes tope

  -- [Heuristic] TODO: check if affects completeness
  -- Make sure new goal is not a trivial consequence of existing premises via LJE
  -- (otherwise this is not useful)
  -- This should work good for rules such as excluded middle for (≤).
  lnot (Prover.RulesM (lift (Prover.proveWithDFS 1 (Prover.fromDefinedRules Prover.rulesLJE) Prover.Sequent{sequentTope = newTope, ..})))

  let goals = map (normalizeSequent . addTopes leftoverTopes . applySubsts substs') premises
      goalR = normalizeSequent $ addTopes [substInTope (substPoints <> substPoints') substTopes tope] currentGoal
      newGoals = goalR : goals

  guard (all (/= currentGoal) newGoals)
  return (name <> " (left)", newGoals)
  where
    addTopes topes Prover.Sequent{..} =
      Prover.Sequent{ sequentTopeContext = topes <> sequentTopeContext, ..}

    isVar (TopeVar _) = True
    isVar _           = False

collectRulePoints :: TopeRule -> Set RSTT.Point
collectRulePoints (TopeRule _ premises _ conclusion) =
  foldMap (collectSequentPoints . convertSequent) (conclusion : premises)

collectSequentPoints :: Prover.Sequent -> Set RSTT.Point
collectSequentPoints (Prover.Sequent _cubeCtx ctx rhs) =
  foldMap collectTopePoints (rhs : ctx)

collectTopePoints :: RSTT.Tope -> Set RSTT.Point
collectTopePoints = go
  where
    go = \case
      RSTT.TopeTop             -> mempty
      RSTT.TopeBottom          -> mempty
      RSTT.TopeVar{}           -> mempty
      RSTT.TopeCon _con points -> Set.fromList points
      RSTT.TopeImplies x y     -> go x <> go y
      RSTT.TopeOr x y          -> go x <> go y
      RSTT.TopeAnd x y         -> go x <> go y
      RSTT.TopeEQ l r          -> Set.fromList [l, r]

collectPointVars :: RSTT.Point -> Set RSTT.Var
collectPointVars = go
  where
    go = \case
      RSTT.PointVar x           -> Set.fromList [x]
      RSTT.PointCon _con points -> Set.unions (go <$> points)
      RSTT.PointUnit            -> mempty
      RSTT.PointPair x y        -> go x <> go y
      RSTT.PointFirst x         -> go x
      RSTT.PointSecond x        -> go x

data Substs = Substs
  { substCubeVars  :: [(RSTT.Var, RSTT.Cube)]
  , substPointVars :: [(RSTT.Var, RSTT.Point)]
  , substTopeVars  :: [(RSTT.Var, RSTT.Tope)]
  }

instance Semigroup Substs where
  Substs cs ps ts <> Substs cs' ps' ts' =
    Substs (cs <> cs') (ps <> ps') (ts <> ts')

matchSequent
  :: (MonadPlus f, MonadFail f)
  => Set RSTT.Var -> Sequent -> Prover.Sequent -> f (Substs, [RSTT.Tope])
matchSequent metavars (Sequent _points topes rhs) (Prover.Sequent _points' topes' rhs') = do
  subst <- matchTope rhs rhs'
  (subst', leftover) <- matchTopeContext subst topes topes'
  subst'' <- matchVars (metavars `Set.difference` Set.fromList (fst <$> (substPointVars subst')))
              (foldMap collectTopePoints (rhs' : topes'))
  return (subst' <> subst'', leftover)

matchVars :: Alternative f => Set RSTT.Var -> Set RSTT.Point -> f Substs
matchVars vars points = Substs []
  <$> traverse (\var -> (,) var <$> Prover.choose (Set.toList points)) (Set.toList vars)
  <*> pure []

matchTopeContext
  :: (MonadPlus f, MonadFail f)
  => Substs -> TopeContext -> RSTT.TopeContext -> f (Substs, RSTT.TopeContext)
matchTopeContext substs ctx ctx' =
  case ctx of
    TopeContextEmpty -> return (substs, ctx')
    TopeContextNonEmpty topes -> do
      matchTopes substs topes ctx'

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

matchTopes
  :: (MonadPlus f, MonadFail f)
  => Substs -> [Tope] -> [RSTT.Tope] -> f (Substs, [RSTT.Tope])
matchTopes substs [] leftover = pure (substs, leftover)
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
normalizeSequent Prover.Sequent{..} = Prover.Sequent
  { sequentCubeContext = nubSort sequentCubeContext
  , sequentTopeContext = nubSort sequentTopeContext
  , .. }

nubSort :: Ord a => [a] -> [a]
nubSort = Set.toList . Set.fromList

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

