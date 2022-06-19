{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
module RSTT.Cube where

import           Data.List   (intercalate)
import           Data.String (IsString (..))

-- | A label (constructor symbol).
newtype Label = Label { getLabel :: String }
  deriving newtype (Eq, Ord, Show, IsString)

-- | A variable.
newtype Var = Var { getVar :: String }
  deriving newtype (Eq, Ord, Show, IsString)

-- | A cube expression.
data Cube
  = CubeUnit                -- ^ Unit cube: \(\mathbb{1}\).
  | CubeProduct Cube Cube   -- ^ Product of cubes: \(I \times J\).
  | CubeCon Label           -- ^ A user-defined cube, e.g. \(\mathbb{2}, \mathbb{I}\).
  | CubeVar Var             -- ^ A cube variable: \(I, J\).
  deriving (Eq, Ord, Show)

instance IsString Cube where
  fromString = CubeVar . fromString

-- | A point in a cube.
data Point
  = PointUnit               -- ^ The only point in the unit cube: \(\star : \mathbb{1}\).
  | PointPair Point Point   -- ^ A pair of points: \(t : I, s : J \vdash \langle t, s \rangle : I \times J\).
  | PointFirst Point        -- ^ First projection: \(\pi_1\;t\).
  | PointSecond Point       -- ^ Second projection: \(\pi_2\;t\).
  | PointCon Label [Point]  -- ^ A user-defined point constructor, e.g. \(0 : \mathbb{2}\) or \(t \land s : \mathbb{I}\).
  | PointVar Var            -- ^ A point variable: \(t, s\).
  deriving (Eq, Ord, Show)

instance IsString Point where
  fromString = PointVar . fromString

-- | A cube context consists of point variables with corresponding cubes.
type CubeContext = [(Var, Cube)]

-- * Pretty-printing

-- | Pretty-print a 'CubeContext'.
ppCubeContext :: CubeContext -> String
ppCubeContext [] = "⋅"
ppCubeContext xs = intercalate ", " (map (\(Var x, c) -> x <> " : " <> ppCube c) xs)

-- | Pretty-print a 'Cube'.
ppCube :: Cube -> String
ppCube = ppCubePrec 0

-- | Pretty-print a 'Cube' expression in a given precedence environment.
ppCubePrec :: Int -> Cube -> String
ppCubePrec prec = \case
  CubeVar (Var x)   -> x
  CubeCon (Label l) -> l
  CubeUnit          -> "𝟙"
  CubeProduct l r   -> binOp ppCubePrec (prec - 1) l "×" r
  where
    parens s = "(" <> s <> ")"
    binOp pp opPrec l op r = (if prec > opPrec then parens else id) $
      pp opPrec l <> " " <> op <> " " <> pp opPrec r

-- | Pretty-print a 'Point'.
ppPoint :: Point -> String
ppPoint = ppPointPrec 0

-- | Pretty-print a 'Point' expression in a given precedence environment.
ppPointPrec :: Int -> Point -> String
ppPointPrec prec = \case
  PointVar (Var x)     -> x
  PointUnit     -> "⋆"
  PointPair l r -> "⟨" <> ppPointPrec prec l <> ", " <> ppPointPrec prec r <> "⟩"
  PointFirst p  -> "𝜋₁(" <> ppPointPrec 0 p <> ")"
  PointSecond p -> "𝜋₂(" <> ppPointPrec 0 p <> ")"
  PointCon (Label l) []  -> l
  PointCon (Label l) args  -> l <> "(" <> intercalate ", " (map (ppPointPrec prec) args) <> ")"

