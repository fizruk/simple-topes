{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
module RSTT.Tope where

import           Data.List   (intercalate)
import           Data.String (IsString (..))

import           RSTT.Cube

-- | A tope expression.
data Tope
  = TopeTop               -- ^ Top tope: \(\top\).
  | TopeBottom            -- ^ Bottom tope: \(\bot\).
  | TopeAnd Tope Tope     -- ^ AND tope: \(\phi \land \psi\).
  | TopeOr  Tope Tope     -- ^ OR tope: \(\phi \lor \psi\).
  | TopeEQ  Point Point   -- ^ Point equality tope: \(t \equiv s\).
  | TopeVar Var           -- ^ A tope variable.
  | TopeImplies Tope Tope -- ^ IMPLIES tope: \(\phi \Rightarrow \psi\).
                          -- NOTE: this tope is added to internalise known tope judgements and simplify proof search.
  | TopeCon Label [Point] -- ^ User-defined tope constructor (fully applied to some points).
  deriving (Eq, Ord, Show)

instance IsString Tope where
  fromString = TopeVar . fromString

-- | A shape is a tope in a cube context (with just one cube point variable):
-- \(\{t : I \mid \phi \}\).
data Shape = Shape
  { shapePoint :: (Var, Cube)
  , shapeTope  :: Tope
  } deriving (Eq, Show)

-- | A tope context consists of a set of topes.
type TopeContext = [Tope]

-- * Pretty-printing

-- | Pretty-print a 'TopeContext'.
ppTopeContext :: TopeContext -> String
ppTopeContext [] = "⋅"
ppTopeContext xs = intercalate ", " (map ppTope xs)

-- | Pretty-print a 'Shape'.
ppShape :: Shape -> String
ppShape Shape{..} = "{" <> t <> " : " <> ppCube cube <> " | " <> ppTope shapeTope <> "}"
  where
    (Var t, cube) = shapePoint

-- | Pretty-print a 'Tope'.
ppTope :: Tope -> String
ppTope = ppTopePrec 0

-- | Pretty-print a 'Tope' expression in a given precedence environment.
ppTopePrec :: Int -> Tope -> String
ppTopePrec prec = \case
  TopeTop         -> "⊤"
  TopeBottom      -> "⊥"
  TopeVar (Var x) -> x
  TopeImplies l r -> binOp ppTopePrec   1 l "⇒" r
  TopeOr      l r -> binOp ppTopePrec   2 l "∨" r
  TopeAnd     l r -> binOp ppTopePrec   3 l "∧" r
  TopeEQ      l r -> binOp ppPointPrec  4 l "≡" r
  TopeCon (Label l) [] -> l
  TopeCon (Label l) args -> l <> "(" <> intercalate ", " (map (ppPointPrec 0) args) <> ")"
  where
    parens s = "(" <> s <> ")"
    binOp pp opPrec l op r = (if prec > opPrec then parens else id) $
      pp opPrec l <> " " <> op <> " " <> pp opPrec r

