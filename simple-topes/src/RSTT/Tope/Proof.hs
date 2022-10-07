{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module RSTT.Tope.Proof where

import           Control.Applicative  (Alternative (..))
import           Data.Bifunctor
#if __GLASGOW_HASKELL__ < 808
import           Control.Monad.Fail   (MonadFail)
#endif
import           Control.Monad.Logic
import           Control.Monad.Reader
import           Data.List            (inits, intercalate, tails)
import           Data.Void

import           RSTT.Cube
import           RSTT.Tope

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import RSTT.Tope

-- * Tope sequent

-- | Tope sequent \( \Xi \mid \Phi \vdash \psi \).
data Sequent = Sequent
  { sequentCubeContext :: CubeContext -- ^ Cube context \(\Xi\).
  , sequentTopeContext :: TopeContext -- ^ Tope context \(\Phi\).
  , sequentTope        :: Tope        -- ^ A tope \(\psi\) on the right hand side.
  } deriving (Eq, Show)

-- | Pretty-print a 'Sequent'.
--
-- >>> points = [("t", "I"), ("s", "J")]
-- >>> topes = [("φ" `TopeOr` "ζ") `TopeAnd` ("ψ" `TopeOr` "χ"), ("t" `TopeEQ` "s")]
-- >>> putStrLn (ppSequent (Sequent points topes ("s" `TopeEQ` "t")))
-- t : I, s : J | (φ ∨ ζ) ∧ (ψ ∨ χ), t ≡ s ⊢ s ≡ t
ppSequent :: Sequent -> String
ppSequent Sequent{..} =
  ppCubeContext sequentCubeContext <> " | " <>
  ppTopeContext sequentTopeContext <> " ⊢ " <>
  ppTope sequentTope

-- * Rules

-- | A name of a (tope inference) rule.
type RuleName = String

-- | A collection of tope inference rules.
-- Intuitively, @'Rules'@ is a non-deterministic function
-- from 'Sequent' to @('RuleName', ['Sequent'])@.
type Rules = RulesM (RuleName, [Sequent])

-- | A collection of generalized tope inference rules.
-- Intuitively, @'RulesM' a@ is a non-deterministic function from 'Sequent' to @a@.
newtype RulesM a = RulesM
  { applyRulesM :: ReaderT Sequent Logic a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadFail)
  deriving (MonadReader Sequent, MonadLogic)

instance Semigroup (RulesM a) where
  RulesM f <> RulesM g = RulesM (f <|> g)

instance Monoid (RulesM a) where
  mempty = RulesM empty

-- | Rules, collected by type.
data DefinedRules = DefinedRules
  { invertibleRules    :: Rules   -- ^ Invertible rules that simplify sequents.
  , tableauxRules      :: Rules   -- ^ Non-invertible tableaux rules.
  , invertibleCutRules :: Rules   -- ^ Invertible rules that add more topes in context.
  , tableauxCutRules   :: Rules   -- ^ Non-invertible tableaux rules that add more topes in context.
  }

instance Semigroup DefinedRules where
  DefinedRules r1 r2 r3 r4 <> DefinedRules r1' r2' r3' r4'
    = DefinedRules (r1 <> r1') (r2 <> r2') (r3 <> r3') (r4 <> r4')

instance Monoid DefinedRules where
  mempty = DefinedRules mempty mempty mempty mempty

fromDefinedRules :: DefinedRules -> Rules
fromDefinedRules DefinedRules{..}
  = once invertibleRules `orElse`
      (tableauxRules <>
        (once invertibleCutRules `orElse` tableauxCutRules))

-- ** Sets of rules

-- | Rules for intuitionistic tope logic:
--
-- * 'axiom' rule
-- * \(\land\)-rules ('leftAnd' and 'rightAnd')
-- * \(\lor\)-rules ('leftOr', 'rightOrL', and 'rightOrR')
-- * \(\Rightarrow\)-rules ('leftImplies', 'rightImplies')
rulesLJ :: DefinedRules
rulesLJ = DefinedRules
  { invertibleRules = mconcat
      [ axiom
      , leftAnd, rightAnd
      , leftOr
      , ruleTop
      , ruleBottom
      , rightImplies
      ]
  , invertibleCutRules = mempty
  , tableauxRules = mconcat
      [ rightOrL, rightOrR
      , leftImplies
      ]
  , tableauxCutRules = mempty
  }

-- | Rules for tope equality:
--
-- * reflexivity ('reflEQ')
-- * symmetry ('symEQ')
-- * transitivity ('transEQ')
-- * substitution ('substEQ')
rulesEQ :: DefinedRules
rulesEQ = DefinedRules
  { invertibleRules     = mconcat [ reflEQ, transEQ, symEQ ]
  , invertibleCutRules  = mempty
  , tableauxRules       = mconcat [ substEQ ]
  , tableauxCutRules    = mempty
  }

-- | Rules for intuitionistic tope logic with equality:
--
-- * intuitionistic rules 'rulesLJ'
-- * equality tope rules 'rulesEQ'
rulesLJE :: DefinedRules
rulesLJE = rulesLJ <> rulesEQ

-- | Rules for inequality tope (not built-in).
--
-- * reflexivity ('reflLEQ')
-- * antisymmetry ('antisymLEQ')
-- * transitivity ('transLEQ')
-- * excluded middle ('lemLEQ')
-- * infimum ('zeroLEQ')
-- * supremum ('oneLEQ')
-- * distinctness of 0 and 1 ('distinctLEQ')
rulesLEQ :: Rules
rulesLEQ = mconcat
  [ reflLEQ
  , transLEQ
  , antisymLEQ
  , lemLEQ
  , zeroLEQ
  , oneLEQ
  , distinctLEQ
  ]

-- ** Intuitionistic logic rules (LJ)

ruleTop :: Rules
ruleTop = do
  TopeTop <- asks sequentTope
  return ("⊤R", [])

ruleBottom :: Rules
ruleBottom = do
  Sequent{..} <- ask
  (TopeBottom, _) <- selectOne sequentTopeContext
  return ("⊥L", [])

leftAnd :: Rules
leftAnd = do
  Sequent{..} <- ask
  (TopeAnd l r, topes) <- selectOne sequentTopeContext
  return ("∧L", [Sequent{ sequentTopeContext = l : r : topes, ..}])

leftOr :: Rules
leftOr = do
  Sequent{..} <- ask
  (TopeOr l r, topes) <- selectOne sequentTopeContext
  return ("∨L",
    [ Sequent{ sequentTopeContext = l : topes, ..}
    , Sequent{ sequentTopeContext = r : topes, ..} ])

leftImplies :: Rules
leftImplies = do
  Sequent{..} <- ask
  (TopeImplies l r, topes) <- selectOne sequentTopeContext
  guard (r `notElem` topes)
  return ("⇒L",
    [ Sequent{ sequentTopeContext = r : sequentTopeContext, ..}
    , Sequent{ sequentTope = l, ..} ])

rightImplies :: Rules
rightImplies = do
  Sequent{..} <- ask
  TopeImplies l r <- return sequentTope
  return ("⇒R",
    [ Sequent{ sequentTope = r, sequentTopeContext = l : sequentTopeContext, ..} ])

rightAnd :: Rules
rightAnd = do
  Sequent{..} <- ask
  TopeAnd l r <- return sequentTope
  return ("∧R",
    [ Sequent{ sequentTope = l, ..}
    , Sequent{ sequentTope = r, ..} ])

rightOrL :: Rules
rightOrL = do
  Sequent{..} <- ask
  TopeOr l _ <- return sequentTope
  return ("∨R₁", [ Sequent{ sequentTope = l, ..} ])

rightOrR :: Rules
rightOrR = do
  Sequent{..} <- ask
  TopeOr _ r <- return sequentTope
  return ("∨R₂", [ Sequent{ sequentTope = r, ..} ])

axiom :: Rules
axiom = do
  Sequent{..} <- ask
  guard (sequentTope `elem` sequentTopeContext)
  return ("Ax", [])

-- ** Equality tope \((\equiv)\) rules

-- | Transitivity rule for equality tope (see 'TopeEQ').
--
-- @
-- ———————————
--  ⋅ ⊢ x ≡ x
-- @
reflEQ :: Rules
reflEQ = do
  TopeEQ l r <- asks sequentTope
  guard (l == r)
  pure ("≡R(refl)", [])

-- | Transitivity rule for equality tope (see 'TopeEQ').
--
-- @
--      x ≡ y, y ≡ z ⊢ φ
-- —————————————————————————
--  x ≡ y, y ≡ z, x ≡ z ⊢ φ
-- @
transEQ :: Rules
transEQ = do
  Sequent{..} <- ask
  (TopeEQ x y, ts) <- selectOne sequentTopeContext
  (TopeEQ y' z, _) <- selectOne ts
  guard (y == y' && x /= z && TopeEQ x z `notElem` sequentTopeContext)
  pure ("≡L(trans)", [Sequent{sequentTopeContext = TopeEQ x z : sequentTopeContext, ..}])

-- | Symmetry rule for equality tope (see 'TopeEQ').
--
-- @
--     x ≡ y ⊢ φ
-- ——————————————————
--  x ≡ y, y ≡ x ⊢ φ
-- @
symEQ :: Rules
symEQ = do
  Sequent{..} <- ask
  (TopeEQ x y, ts) <- selectOne sequentTopeContext
  guard (x /= y && TopeEQ y x `notElem` ts)
  pure ("≡L(sym)", [Sequent{sequentTopeContext = TopeEQ y x : sequentTopeContext, ..}])

-- | Substitution rule for equality tope (see 'TopeEQ').
--
-- @
--    Г, x ≡ y ⊢ φ
-- —————————————————
--  Г[y/x] ⊢ φ[y/x]
-- @
--
-- NOTE: limited to replacing a variable, not an arbitrary point subterm.
substEQ :: Rules
substEQ = do
  Sequent{..} <- ask
  (TopeEQ x y, ts) <- selectOne sequentTopeContext
  guard (isVar x) -- TODO: generalize
  guard (not (x `subPointOf` y))
  pure ("≡L(subst)", [Sequent
    { sequentTopeContext = map (replacePointInTope x y) ts
    , sequentTope = replacePointInTope x y sequentTope
    , .. }])
  where
    isVar PointVar{} = True
    isVar _          = False

-- ** Inequality tope \((\leq)\) rules

reflLEQ :: Rules
reflLEQ = do
  TopeCon (Label "≤") [x, y] <- asks sequentTope
  guard (x == y)
  pure ("≤R(refl)", [])

transLEQ :: Rules
transLEQ = do
  Sequent{..} <- ask
  (TopeCon (Label "≤") [x, y], ts) <- selectOne sequentTopeContext
  (TopeCon (Label "≤") [y', z], _) <- selectOne ts
  guard (y == y' && x /= z && TopeCon (Label "≤") [x, z] `notElem` sequentTopeContext)
  pure ("≤L(trans)", [Sequent{sequentTopeContext = TopeCon (Label "≤") [x, z] : sequentTopeContext, ..}])

antisymLEQ :: Rules
antisymLEQ = do
  Sequent{..} <- ask
  (TopeCon (Label "≤") [x, y], ts) <- selectOne sequentTopeContext
  (TopeCon (Label "≤") [y', x'], _) <- selectOne ts
  guard (y == y' && x == x')
  pure ("≤L(antisym)", [Sequent{sequentTopeContext = TopeEQ x y : sequentTopeContext, ..}])

lemLEQ :: Rules
lemLEQ = do
  TopeOr (TopeCon (Label "≤") [x, y]) (TopeCon (Label "≤") [y', x']) <- asks sequentTope
  guard (x == x' && y == y')
  pure ("≤R(lem)", [])

zeroLEQ :: Rules
zeroLEQ = do
  TopeCon (Label "≤") [PointCon "0" [], _] <- asks sequentTope
  pure ("≤R(zero)", [])

oneLEQ :: Rules
oneLEQ = do
  TopeCon (Label "≤") [_, PointCon "1" []] <- asks sequentTope
  pure ("≤R(one)", [])

distinctLEQ :: Rules
distinctLEQ = do
  (TopeEQ (PointCon "0" []) (PointCon "1" []), _) <- asks sequentTopeContext >>= selectOne
  pure ("≤L(distinct)", [])

-- ** Helpers

-- | Try the second computation only if the first one fails (i.e. no backtracking).
--
-- The following examples show the difference between 'orElse' and 'mplus' for 'Logic':
--
-- >>> observeAll (pure 1 `orElse` pure 2)
-- [1]
-- >>> observeAll (pure 1 `mplus` pure 2)
-- [1,2]
--
-- >>> observeAll $ do { x <- pure 1 `orElse` pure 2; guard (even x); return x }
-- []
-- >>> observeAll $ do { x <- pure 1 `mplus` pure 2; guard (even x); return x }
-- [2]
orElse :: MonadLogic m => m a -> m a -> m a
orElse mx my =
  msplit mx >>= \case
    Nothing      -> my
    Just (x, xs) -> pure x <|> xs

-- | Nondeterministically select exactly \(N\) elements from a given list,
-- also returning the remaining part of the list.
selectN :: Alternative f => Int -> [a] -> f ([a], [a])
selectN n = choose . splits n
  where
    splits k as
      | k <= 0 = [([], as)]
      | otherwise =
          [ (y:ys, xs ++ zs')
          | (xs, y:zs) <- zip (inits as) (tails as)
          , (ys, zs') <- splits (k - 1) zs
          ]

-- | Nondeterministically select exactly one element from a given list,
-- also returning the remaining part of the list.
selectOne :: (Alternative f, MonadFail f) => [a] -> f (a, [a])
selectOne xs = do
  ([y], zs) <- selectN 1 xs
  return (y, zs)

-- | Nondeterministically select exactly one element from a given list.
choose :: Alternative f => [a] -> f a
choose = foldr (<|>) empty . map pure

-- * Proof tree

-- | A complete proof is a proof tree without any unfinished parts.
type Proof = ProofTree Void

-- | A proof tree parametrized by the type of leaves (unfinished proofs).
data ProofTree a
  = Leaf a
  -- ^ A leaf represents an unfinished part of a proof.
  | Node Sequent RuleName [ProofTree a]
  -- ^ A node in a proof tree corresponds to a single rule application.
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative ProofTree where
  pure = return
  (<*>) = ap

instance Monad ProofTree where
  return = Leaf
  Leaf x >>= f      = f x
  Node s r ts >>= f = Node s r (map (>>= f) ts)

-- | Convert a proof tree into a complete proof if possible.
close :: Alternative f => ProofTree a -> f Proof
close = traverse (const empty)

-- ** ASCII rendering

-- | Pretty print a proof tree (in ASCII form).
ppProof :: Proof -> String
ppProof = ppNodesWithDepth . proofNodesWithDepth

-- | Extract proof nodes with corresponding depths.
proofNodesWithDepth :: Proof -> [(Int, String)]
proofNodesWithDepth = go 0
  where
    go i = \case
      Node s r ts -> (i, "[" <> r <> "]" <> "  " <> ppSequent s) : concatMap (go (i + 1)) ts
      Leaf x      -> absurd x

-- | Pretty-print depth-annotated nodes as a ASCII tree-like structure.
--
-- >>> putStr $ ppNodesWithDepth (zip [0,1,2,1,2,3,3,2,3] (map show [0..]))
-- 0
-- ├─ 1
-- │  └─ 2
-- └─ 3
--    ├─ 4
--    │  ├─ 5
--    │  └─ 6
--    └─ 7
--       └─ 8
ppNodesWithDepth :: [(Int, String)] -> String
ppNodesWithDepth xs = intercalate "\n" $
  zipWith (\t x -> drop 3 $ t <> " " <> snd x) (depthsTree (map fst xs)) xs

-- | Pretty-print an ASCII tree-like structure based on depth list.
--
-- >>> mapM_ putStrLn $ depthsTree [0,1,2,1,2,3,3,2,3]
-- └─
--    ├─
--    │  └─
--    └─
--       ├─
--       │  ├─
--       │  └─
--       └─
--          └─
depthsTree :: [Int] -> [String]
depthsTree = map (<> "─") . scanr1 prev . map defaultLine
  where
    defaultLine i = replicate (3 * i) ' ' <> "└"

    prev i []   = i
    prev i next = zipWith g i (next ++ repeat ' ')
      where
        g ' ' '│' = '│'
        g ' ' '└' = '│'
        g '─' '└' = '┬'
        g '└' '│' = '├'
        g '└' '└' = '├'
        g '─' '├' = '┬'
        g c _     = c

-- * Provers

-- | A simple DFS proof search algorithm for a given set of rules.
proveWithDFS
  :: Int                        -- ^ Maximum search depth.
  -> Rules                      -- ^ Set of rules.
  -> Sequent                    -- ^ Starting sequent (one to prove).
  -> Logic (ProofTree Sequent)  -- ^ A proof tree, possibly with unproven sequents in leaves
                                -- (for when maximum search depth is insufficient).
proveWithDFS maxDepth rules sequent =
  join <$> traverse (f maxDepth) (Leaf sequent)
  where
    f n s
      | n <= 0 = return (Leaf s)
      | otherwise = do
          (r, ss) <- runReaderT (applyRulesM rules) s
          ss' <- traverse (f (n - 1)) ss
          return (Node s r ss')

-- | A simple k-depth proof search algorithm for a given set of rules.
proveWithBFSviaDFS
  :: Int          -- ^ Maximum number of limited DFS iterations.
  -> Int          -- ^ Maximum depth per one DFS iteration.
  -> Rules        -- ^ A set of rules.
  -> Sequent      -- ^ A starting sequent (one to prove).
  -> Logic Proof
proveWithBFSviaDFS maxDepth k rules sequent =
  go (1 + maxDepth) (pure (Leaf sequent))
  where
    go n t
      | n <= 0 = empty
      | otherwise = do
          tryClose t >>= \case
            Right proof -> pure proof
            Left [] -> empty
            Left incompletes -> {- trace (unlines $
              [ "Trying at level " <> show (maxDepth - n + 1) <> " (with k=" <> show k <> "), having " <> show (length incompletes) <> " partial proofs:"
              , unlines (map (ppProof . markIncomplete) $ take 10 incompletes)
              ]) $ -}
              go (n - 1) $ do
                join <$> (choose incompletes >>= traverse (proveWithDFS k rules))

markIncomplete :: ProofTree Sequent -> Proof
markIncomplete = \case
  Leaf x         -> Node x "incomplete" []
  Node x name ts -> Node x name (markIncomplete <$> ts)

tryClose :: Logic (ProofTree a) -> Logic (Either [ProofTree a] Proof)
tryClose m =
  msplit m >>= \case
    Nothing      -> pure (Left [])
    Just (x, xs) ->
          fmap Right (close x)
      <|> fmap (first (x:)) (tryClose xs)

-- | Search for a proof using simple DFS algorithm (see 'proveWithDFS').
proveWithDFS'
  :: Int                        -- ^ Maximum search depth.
  -> Rules                      -- ^ Set of rules.
  -> Sequent                    -- ^ Starting sequent (one to prove).
  -> Maybe Proof
proveWithDFS' maxDepth rules sequent =
  case observeMany 1 (proveWithDFS maxDepth rules sequent >>= close) of
    []  -> Nothing
    p:_ -> Just p

-- | Search for a proof using a combination of BFS and DFS algorithms (see 'proveWithBFS').
proveWithBFSviaDFS'
  :: Int          -- ^ Maximum number of limited DFS iterations.
  -> Int          -- ^ Maximum depth per one DFS iteration.
  -> Rules        -- ^ A set of rules.
  -> Sequent      -- ^ A starting sequent (one to prove).
  -> Maybe Proof
proveWithBFSviaDFS' maxDepth k rules sequent =
  case observeMany 1 (proveWithBFSviaDFS maxDepth k rules sequent) of
    []  -> Nothing
    p:_ -> Just p

-- ** With printers

-- | Search for a proof using BFS and print proof tree (see 'ppProof').
proveAndPrintBFS
  :: Int                        -- ^ Maximum search depth.
  -> Rules                      -- ^ Set of rules.
  -> Sequent                    -- ^ Starting sequent (one to prove).
  -> IO ()
proveAndPrintBFS maxDepth = proveAndPrintBFSviaDFS maxDepth 1

-- | Search for a proof using a combination of BFS and DFS
-- and print proof tree (see 'ppProof').
proveAndPrintBFSviaDFS :: Int -> Int -> Rules -> Sequent -> IO ()
proveAndPrintBFSviaDFS maxDepth k rules sequent = do
  case proveWithBFSviaDFS' maxDepth k rules sequent of
    Just p  -> putStrLn (ppProof p)
    Nothing -> putStrLn ("The sequent is not provable: " <> ppSequent sequent)

-- | Search for a proof using DFS and print proof tree (see 'ppProof').
proveAndPrintDFS :: Int -> Rules -> Sequent -> IO ()
proveAndPrintDFS maxDepth rules sequent = do
  case proveWithDFS' maxDepth rules sequent of
    Just p  -> putStrLn (ppProof p)
    Nothing -> putStrLn ("The sequent is not provable: " <> ppSequent sequent)

-- * Examples

-- |
-- >>> putStrLn (ppSequent ex1)
-- ⋅ | (φ ∨ ζ) ∧ (ψ ∨ χ) ⊢ φ ∧ ψ ∨ ζ ∧ ψ ∨ χ
--
-- >>> proveAndPrintBFS 8 (fromDefinedRules rulesLJ) ex1
-- [∧L]  ⋅ | (φ ∨ ζ) ∧ (ψ ∨ χ) ⊢ φ ∧ ψ ∨ ζ ∧ ψ ∨ χ
-- └─ [∨L]  ⋅ | φ ∨ ζ, ψ ∨ χ ⊢ φ ∧ ψ ∨ ζ ∧ ψ ∨ χ
--    ├─ [∨L]  ⋅ | φ, ψ ∨ χ ⊢ φ ∧ ψ ∨ ζ ∧ ψ ∨ χ
--    │  ├─ [∨R₁]  ⋅ | ψ, φ ⊢ φ ∧ ψ ∨ ζ ∧ ψ ∨ χ
--    │  │  └─ [∧R]  ⋅ | ψ, φ ⊢ φ ∧ ψ
--    │  │     ├─ [Ax]  ⋅ | ψ, φ ⊢ φ
--    │  │     └─ [Ax]  ⋅ | ψ, φ ⊢ ψ
--    │  └─ [∨R₂]  ⋅ | χ, φ ⊢ φ ∧ ψ ∨ ζ ∧ ψ ∨ χ
--    │     └─ [∨R₂]  ⋅ | χ, φ ⊢ ζ ∧ ψ ∨ χ
--    │        └─ [Ax]  ⋅ | χ, φ ⊢ χ
--    └─ [∨L]  ⋅ | ζ, ψ ∨ χ ⊢ φ ∧ ψ ∨ ζ ∧ ψ ∨ χ
--       ├─ [∨R₂]  ⋅ | ψ, ζ ⊢ φ ∧ ψ ∨ ζ ∧ ψ ∨ χ
--       │  └─ [∨R₁]  ⋅ | ψ, ζ ⊢ ζ ∧ ψ ∨ χ
--       │     └─ [∧R]  ⋅ | ψ, ζ ⊢ ζ ∧ ψ
--       │        ├─ [Ax]  ⋅ | ψ, ζ ⊢ ζ
--       │        └─ [Ax]  ⋅ | ψ, ζ ⊢ ψ
--       └─ [∨R₂]  ⋅ | χ, ζ ⊢ φ ∧ ψ ∨ ζ ∧ ψ ∨ χ
--          └─ [∨R₂]  ⋅ | χ, ζ ⊢ ζ ∧ ψ ∨ χ
--             └─ [Ax]  ⋅ | χ, ζ ⊢ χ
ex1 :: Sequent
ex1 = Sequent []
  [ ("φ" `TopeOr` "ζ") `TopeAnd` ("ψ" `TopeOr` "χ") ]
  (("φ" `TopeAnd` "ψ") `TopeOr` (("ζ" `TopeAnd` "ψ") `TopeOr` "χ"))

-- |
-- >>> putStrLn (ppSequent ex2)
-- ⋅ | φ ∧ ψ ∨ ζ ∧ χ ⊢ (φ ∨ ζ) ∧ (ψ ∨ χ)
--
-- >>> proveAndPrintBFS 8 (fromDefinedRules rulesLJ) ex2
-- [∧R]  ⋅ | φ ∧ ψ ∨ ζ ∧ χ ⊢ (φ ∨ ζ) ∧ (ψ ∨ χ)
-- ├─ [∨L]  ⋅ | φ ∧ ψ ∨ ζ ∧ χ ⊢ φ ∨ ζ
-- │  ├─ [∧L]  ⋅ | φ ∧ ψ ⊢ φ ∨ ζ
-- │  │  └─ [∨R₁]  ⋅ | φ, ψ ⊢ φ ∨ ζ
-- │  │     └─ [Ax]  ⋅ | φ, ψ ⊢ φ
-- │  └─ [∧L]  ⋅ | ζ ∧ χ ⊢ φ ∨ ζ
-- │     └─ [∨R₂]  ⋅ | ζ, χ ⊢ φ ∨ ζ
-- │        └─ [Ax]  ⋅ | ζ, χ ⊢ ζ
-- └─ [∨L]  ⋅ | φ ∧ ψ ∨ ζ ∧ χ ⊢ ψ ∨ χ
--    ├─ [∧L]  ⋅ | φ ∧ ψ ⊢ ψ ∨ χ
--    │  └─ [∨R₁]  ⋅ | φ, ψ ⊢ ψ ∨ χ
--    │     └─ [Ax]  ⋅ | φ, ψ ⊢ ψ
--    └─ [∧L]  ⋅ | ζ ∧ χ ⊢ ψ ∨ χ
--       └─ [∨R₂]  ⋅ | ζ, χ ⊢ ψ ∨ χ
--          └─ [Ax]  ⋅ | ζ, χ ⊢ χ
ex2 :: Sequent
ex2 = Sequent []
  [ ("φ" `TopeAnd` "ψ") `TopeOr` ("ζ" `TopeAnd` "χ") ]
  (("φ" `TopeOr` "ζ") `TopeAnd` ("ψ" `TopeOr` "χ"))

-- |
-- >>> putStrLn (ppSequent ex3)
-- ⋅ | x ≡ y ∧ y ≡ z ⊢ z ≡ x
--
-- >>> proveAndPrintBFS 35 (fromDefinedRules rulesLJE) ex3
-- [∧L]  ⋅ | x ≡ y ∧ y ≡ z ⊢ z ≡ x
-- └─ [≡L(trans)]  ⋅ | x ≡ y, y ≡ z ⊢ z ≡ x
--    └─ [≡L(sym)]  ⋅ | x ≡ z, x ≡ y, y ≡ z ⊢ z ≡ x
--       └─ [Ax]  ⋅ | z ≡ x, x ≡ z, x ≡ y, y ≡ z ⊢ z ≡ x
ex3 :: Sequent
ex3 = Sequent []
  [ TopeEQ "x" "y" `TopeAnd` TopeEQ "y" "z" ]
  (TopeEQ "z" "x")

-- |
-- >>> putStrLn (ppSequent ex4)
-- ⋅ | (t ≡ s ∨ t ≡ u) ∧ s ≡ u ⊢ t ≡ s
--
-- >>> proveAndPrintBFS 8 (fromDefinedRules rulesLJE) ex4
-- [∧L]  ⋅ | (t ≡ s ∨ t ≡ u) ∧ s ≡ u ⊢ t ≡ s
-- └─ [∨L]  ⋅ | t ≡ s ∨ t ≡ u, s ≡ u ⊢ t ≡ s
--    ├─ [Ax]  ⋅ | t ≡ s, s ≡ u ⊢ t ≡ s
--    └─ [≡L(sym)]  ⋅ | t ≡ u, s ≡ u ⊢ t ≡ s
--       └─ [≡L(trans)]  ⋅ | u ≡ t, t ≡ u, s ≡ u ⊢ t ≡ s
--          └─ [≡L(sym)]  ⋅ | s ≡ t, u ≡ t, t ≡ u, s ≡ u ⊢ t ≡ s
--             └─ [Ax]  ⋅ | t ≡ s, s ≡ t, u ≡ t, t ≡ u, s ≡ u ⊢ t ≡ s
ex4 :: Sequent
ex4 = Sequent []
  [ (TopeEQ "t" "s" `TopeOr` TopeEQ "t" "u") `TopeAnd` TopeEQ "s" "u" ]
  (TopeEQ "t" "s")

-- |
-- >>> putStrLn (ppSequent ex5)
-- ⋅ | (≤(t, s) ∨ ≤(s, u)) ∧ ≤(t, u) ⊢ ≤(t, u) ∨ ≤(s, t) ∨ ≤(u, s)
--
-- >>> proveAndPrintBFS 5 (fromDefinedRules rulesLJE) ex5
-- [∧L]  ⋅ | (≤(t, s) ∨ ≤(s, u)) ∧ ≤(t, u) ⊢ ≤(t, u) ∨ ≤(s, t) ∨ ≤(u, s)
-- └─ [∨L]  ⋅ | ≤(t, s) ∨ ≤(s, u), ≤(t, u) ⊢ ≤(t, u) ∨ ≤(s, t) ∨ ≤(u, s)
--    ├─ [∨R₁]  ⋅ | ≤(t, s), ≤(t, u) ⊢ ≤(t, u) ∨ ≤(s, t) ∨ ≤(u, s)
--    │  └─ [∨R₁]  ⋅ | ≤(t, s), ≤(t, u) ⊢ ≤(t, u) ∨ ≤(s, t)
--    │     └─ [Ax]  ⋅ | ≤(t, s), ≤(t, u) ⊢ ≤(t, u)
--    └─ [∨R₁]  ⋅ | ≤(s, u), ≤(t, u) ⊢ ≤(t, u) ∨ ≤(s, t) ∨ ≤(u, s)
--       └─ [∨R₁]  ⋅ | ≤(s, u), ≤(t, u) ⊢ ≤(t, u) ∨ ≤(s, t)
--          └─ [Ax]  ⋅ | ≤(s, u), ≤(t, u) ⊢ ≤(t, u)
ex5 :: Sequent
ex5 = Sequent []
  [ (TopeCon "≤" ["t", "s"] `TopeOr` TopeCon "≤" ["s", "u"]) `TopeAnd` TopeCon "≤" ["t", "u"] ]
  (TopeCon "≤" ["t", "u"] `TopeOr` TopeCon "≤" ["s", "t"] `TopeOr` TopeCon "≤" ["u", "s"])

