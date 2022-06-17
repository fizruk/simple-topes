{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module RSTT.Syntax.Par
  ( happyError
  , myLexer
  , pProgram
  , pDecl
  , pListDecl
  , pPointConDecl
  , pListPointConDecl
  , pCube
  , pCube1
  , pListCube
  , pTopeRule
  , pListTopeRule
  , pRuleName
  , pSequent
  , pListSequent
  , pCubeContext
  , pPointDecl
  , pListPointDecl
  , pTopeContext
  , pTope
  , pTope1
  , pTope2
  , pTope3
  , pListTope
  , pPoint
  , pListPoint
  ) where

import Prelude

import qualified RSTT.Syntax.Abs
import RSTT.Syntax.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap27 = HappyWrap27 (String)
happyIn27 :: (String) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 (RSTT.Syntax.Abs.Label)
happyIn28 :: (RSTT.Syntax.Abs.Label) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 (RSTT.Syntax.Abs.Var)
happyIn29 :: (RSTT.Syntax.Abs.Var) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (RSTT.Syntax.Abs.Line)
happyIn30 :: (RSTT.Syntax.Abs.Line) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (RSTT.Syntax.Abs.Program)
happyIn31 :: (RSTT.Syntax.Abs.Program) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (RSTT.Syntax.Abs.Decl)
happyIn32 :: (RSTT.Syntax.Abs.Decl) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 ([RSTT.Syntax.Abs.Decl])
happyIn33 :: ([RSTT.Syntax.Abs.Decl]) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 (RSTT.Syntax.Abs.PointConDecl)
happyIn34 :: (RSTT.Syntax.Abs.PointConDecl) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 ([RSTT.Syntax.Abs.PointConDecl])
happyIn35 :: ([RSTT.Syntax.Abs.PointConDecl]) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 (RSTT.Syntax.Abs.Cube)
happyIn36 :: (RSTT.Syntax.Abs.Cube) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 (RSTT.Syntax.Abs.Cube)
happyIn37 :: (RSTT.Syntax.Abs.Cube) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 ([RSTT.Syntax.Abs.Cube])
happyIn38 :: ([RSTT.Syntax.Abs.Cube]) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (RSTT.Syntax.Abs.TopeRule)
happyIn39 :: (RSTT.Syntax.Abs.TopeRule) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 ([RSTT.Syntax.Abs.TopeRule])
happyIn40 :: ([RSTT.Syntax.Abs.TopeRule]) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (RSTT.Syntax.Abs.RuleName)
happyIn41 :: (RSTT.Syntax.Abs.RuleName) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (RSTT.Syntax.Abs.Sequent)
happyIn42 :: (RSTT.Syntax.Abs.Sequent) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 ([RSTT.Syntax.Abs.Sequent])
happyIn43 :: ([RSTT.Syntax.Abs.Sequent]) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (RSTT.Syntax.Abs.CubeContext)
happyIn44 :: (RSTT.Syntax.Abs.CubeContext) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (RSTT.Syntax.Abs.PointDecl)
happyIn45 :: (RSTT.Syntax.Abs.PointDecl) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 ([RSTT.Syntax.Abs.PointDecl])
happyIn46 :: ([RSTT.Syntax.Abs.PointDecl]) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (RSTT.Syntax.Abs.TopeContext)
happyIn47 :: (RSTT.Syntax.Abs.TopeContext) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 (RSTT.Syntax.Abs.Tope)
happyIn48 :: (RSTT.Syntax.Abs.Tope) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 (RSTT.Syntax.Abs.Tope)
happyIn49 :: (RSTT.Syntax.Abs.Tope) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (RSTT.Syntax.Abs.Tope)
happyIn50 :: (RSTT.Syntax.Abs.Tope) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (RSTT.Syntax.Abs.Tope)
happyIn51 :: (RSTT.Syntax.Abs.Tope) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 ([RSTT.Syntax.Abs.Tope])
happyIn52 :: ([RSTT.Syntax.Abs.Tope]) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 (RSTT.Syntax.Abs.Point)
happyIn53 :: (RSTT.Syntax.Abs.Point) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 ([RSTT.Syntax.Abs.Point])
happyIn54 :: ([RSTT.Syntax.Abs.Point]) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x34\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x40\x00\x00\xf8\x66\x00\x00\x00\x00\x00\x00\x80\x00\x00\xb0\xcd\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x9b\x01\x00\x00\x00\x00\x00\x00\x02\x00\x00\x36\x03\x00\x00\x00\x00\x00\x00\x04\x00\x00\x6c\x06\x00\x00\x00\x00\x00\x00\x08\x00\x00\xdb\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x33\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x33\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\xdb\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x10\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x40\x03\x00\x00\x00\x00\x00\x00\x08\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\xf8\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x02\x00\x00\x40\x03\x00\x00\x00\x00\x00\x00\x04\x00\x00\x6c\x06\x00\x00\x00\x00\x00\x00\x08\x00\x00\xd8\x0c\x00\x00\x00\x00\x00\x00\x10\x00\x00\xb0\x19\x00\x00\x00\x00\x00\x00\x40\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\x66\x00\x00\x00\x00\x00\x00\x80\x00\x00\xb0\xcd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x36\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6c\x06\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb0\x19\x00\x00\x00\x00\x00\x00\x00\x00\x00\x60\x33\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9b\x01\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\xa0\x01\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x20\x00\x00\x00\x00\x00\x00\x40\x00\x00\xd8\x66\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x40\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pDecl","%start_pListDecl","%start_pPointConDecl","%start_pListPointConDecl","%start_pCube","%start_pCube1","%start_pListCube","%start_pTopeRule","%start_pListTopeRule","%start_pRuleName","%start_pSequent","%start_pListSequent","%start_pCubeContext","%start_pPointDecl","%start_pListPointDecl","%start_pTopeContext","%start_pTope","%start_pTope1","%start_pTope2","%start_pTope3","%start_pListTope","%start_pPoint","%start_pListPoint","String","Label","Var","Line","Program","Decl","ListDecl","PointConDecl","ListPointConDecl","Cube","Cube1","ListCube","TopeRule","ListTopeRule","RuleName","Sequent","ListSequent","CubeContext","PointDecl","ListPointDecl","TopeContext","Tope","Tope1","Tope2","Tope3","ListTope","Point","ListPoint","'('","')'","','","':'","';'","'cube'","'point'","'prove'","'rule'","'tope'","'where'","'with'","'{'","'|'","'}'","'\215'","'\8658'","'\8743'","'\8744'","'\8801'","'\8866'","'\8868'","'\8869'","'\8901'","'\8902'","'\10216'","'\10217'","'\120587\8321'","'\120587\8322'","'\120793'","L_quoted","L_Label","L_Var","L_Line","%eof"]
        bit_start = st Prelude.* 89
        bit_end = (st Prelude.+ 1) Prelude.* 89
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..88]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x42\x00\x42\x00\x42\x00\x29\x00\x29\x00\x11\x00\x11\x00\x11\x00\x2d\x00\x2d\x00\x2c\x00\x5b\x00\x5b\x00\x5b\x00\x28\x00\x28\x00\x01\x00\x0d\x00\x1b\x00\x1b\x00\x1b\x00\x0d\x00\x0f\x01\x0f\x01\x30\x00\x00\x00\x71\x00\x00\x00\x76\x00\x5a\x00\x00\x00\x0f\x01\x91\x00\x9d\x00\x00\x00\x00\x00\x82\x00\xa8\x00\xa3\x00\x6e\x00\xbb\x00\x9a\x00\x00\x00\xb1\x00\xa7\x00\x0d\x00\x00\x00\x00\x00\xb4\x00\xf3\xff\x20\x00\xf2\xff\xb4\x00\x00\x00\x00\x00\xd5\x00\xde\x00\xdc\x00\xdc\x00\xdc\x00\x00\x00\x00\x00\xe6\x00\xe0\x00\xdf\x00\x03\x01\x00\x00\x03\x01\xf3\x00\x07\x01\xc4\x00\x16\x01\x00\x00\x00\x00\xaf\x00\x00\x00\x16\x01\x11\x00\x00\x00\x16\x01\xf1\xff\x04\x01\x18\x01\xec\x00\x1a\x01\x35\x01\x1c\x01\x1e\x01\x2f\x01\x20\x01\x1f\x01\x1f\x01\x00\x00\x40\x01\x5b\x00\x37\x01\x42\x00\x43\x01\x3e\x01\x11\x00\x40\x00\x11\x00\x3b\x01\x3f\x01\x01\x00\x5b\x00\x26\x01\x11\x00\x1b\x00\x1b\x00\x1b\x00\x02\x00\x0f\x01\x0d\x00\x0f\x01\x0f\x01\x0f\x01\x46\x01\x0f\x01\x0f\x01\x48\x01\x00\x00\x0f\x01\x49\x01\x4a\x01\x4b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x3c\x01\x3d\x01\x41\x01\x00\x00\x00\x00\x3a\x01\x00\x00\x45\x01\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x47\x01\x44\x01\x11\x00\x53\x01\x00\x00\x4f\x01\x55\x01\x5b\x00\x0d\x00\x4c\x01\x00\x00\x00\x00\x42\x01\x00\x00\x00\x00\x4d\x01\x36\x01\x00\x00\x50\x01\x4e\x01\x54\x01\x00\x00\x56\x01\x00\x00\x5b\x00\x59\x01\x57\x01\x58\x01\x00\x00\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\xee\x00\x5e\x01\x9c\x00\x52\x01\x03\x00\x17\x01\x07\x00\xfb\x00\x5c\x01\xe9\x00\x70\x00\xdd\x00\xc0\x00\x65\x00\x3f\x00\x8a\x00\x45\x00\x73\x00\x99\x00\xa2\x00\xa9\x00\x53\x00\x0b\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x98\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x19\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5b\x01\x00\x00\x00\x00\x00\x00\x63\x01\x00\x00\x64\x01\x00\x00\x00\x00\x00\x00\x00\x00\xe8\x00\x00\x00\x28\x01\x00\x00\x2a\x01\x1d\x01\x00\x00\x06\x01\x00\x00\x27\x01\x4c\x00\xcb\x00\xc3\x00\x1b\x01\x9e\x00\xa6\x00\xac\x00\x00\x00\x3e\x00\x6d\x00\x2a\x00\xc6\x00\xc8\x00\x00\x00\x38\x00\x3c\x00\x00\x00\x00\x00\xca\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x09\x01\x00\x00\x00\x00\x00\x00\x0c\x01\x00\x00\x00\x00\x2e\x01\x00\x00\xd6\x00\x7f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x66\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xef\x00\x2b\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xdf\xff\x00\x00\xdf\xff\x00\x00\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\xce\xff\x00\x00\x00\x00\xc9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe7\xff\xac\xff\xab\xff\xaa\xff\x00\x00\xb1\xff\x00\x00\x00\x00\x00\x00\xe6\xff\xe5\xff\x00\x00\xac\xff\xb5\xff\xb3\xff\xbc\xff\xba\xff\xb8\xff\x00\x00\x00\x00\x00\x00\xbf\xff\xbe\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc0\xff\xc1\xff\x00\x00\xc3\xff\x00\x00\x00\x00\x00\x00\xc5\xff\xc6\xff\xc8\xff\x00\x00\x00\x00\x00\x00\xcb\xff\x00\x00\xcd\xff\x00\x00\x00\x00\x00\x00\xd4\xff\xd3\xff\xd1\xff\xd6\xff\x00\x00\x00\x00\xd5\xff\x00\x00\x00\x00\xd9\xff\x00\x00\x00\x00\x00\x00\xde\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe3\xff\x00\x00\x00\x00\x00\x00\xdf\xff\xdc\xff\xda\xff\x00\x00\x00\x00\x00\x00\x00\x00\xce\xff\x00\x00\xc9\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa9\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb2\xff\xb7\xff\xb4\xff\xb9\xff\xbb\xff\xbd\xff\xc4\xff\xc2\xff\xc7\xff\x00\x00\xcc\xff\x00\x00\xd0\xff\xd2\xff\xd7\xff\xd8\xff\x00\x00\xdd\xff\x00\x00\x00\x00\x00\x00\x00\x00\xe0\xff\xda\xff\x00\x00\xc9\xff\x00\x00\xb6\xff\xae\xff\xaf\xff\x00\x00\xad\xff\xb0\xff\xca\xff\x00\x00\xdb\xff\x00\x00\x00\x00\x00\x00\xe2\xff\x00\x00\xe4\xff\x00\x00\xce\xff\x00\x00\x00\x00\xcf\xff\xe1\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x10\x00\x01\x00\x11\x00\x02\x00\x12\x00\x01\x00\x02\x00\x01\x00\x02\x00\x07\x00\x08\x00\x01\x00\x02\x00\x01\x00\x01\x00\x02\x00\x0a\x00\x01\x00\x11\x00\x23\x00\x23\x00\x23\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x01\x00\x1c\x00\x1d\x00\x1a\x00\x1b\x00\x20\x00\x21\x00\x16\x00\x17\x00\x1a\x00\x19\x00\x1a\x00\x1a\x00\x1c\x00\x1d\x00\x01\x00\x02\x00\x20\x00\x21\x00\x1e\x00\x07\x00\x20\x00\x21\x00\x13\x00\x19\x00\x1a\x00\x09\x00\x1c\x00\x1d\x00\x01\x00\x02\x00\x20\x00\x21\x00\x01\x00\x02\x00\x01\x00\x02\x00\x02\x00\x02\x00\x23\x00\x1a\x00\x1b\x00\x01\x00\x02\x00\x06\x00\x21\x00\x08\x00\x1f\x00\x0a\x00\x01\x00\x02\x00\x1f\x00\x10\x00\x12\x00\x1a\x00\x1b\x00\x01\x00\x02\x00\x1a\x00\x1b\x00\x1a\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x14\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x02\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x01\x00\x02\x00\x00\x00\x03\x00\x01\x00\x18\x00\x01\x00\x02\x00\x11\x00\x12\x00\x13\x00\x03\x00\x01\x00\x02\x00\x21\x00\x23\x00\x0e\x00\x11\x00\x01\x00\x02\x00\x15\x00\x16\x00\x17\x00\x18\x00\x19\x00\x1a\x00\x15\x00\x16\x00\x17\x00\x18\x00\x02\x00\x1a\x00\x15\x00\x16\x00\x17\x00\x18\x00\x01\x00\x1a\x00\x15\x00\x16\x00\x17\x00\x18\x00\x00\x00\x1a\x00\x01\x00\x02\x00\x12\x00\x13\x00\x01\x00\x01\x00\x02\x00\x05\x00\x06\x00\x01\x00\x02\x00\x23\x00\x0e\x00\x01\x00\x02\x00\x01\x00\x01\x00\x02\x00\x12\x00\x01\x00\x02\x00\x16\x00\x17\x00\x18\x00\x03\x00\x1a\x00\x16\x00\x17\x00\x18\x00\x14\x00\x1a\x00\x17\x00\x18\x00\x14\x00\x1a\x00\x17\x00\x18\x00\x10\x00\x1a\x00\x18\x00\x02\x00\x1a\x00\x18\x00\x02\x00\x1a\x00\x01\x00\x02\x00\x01\x00\x02\x00\x01\x00\x02\x00\x02\x00\x13\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x23\x00\x12\x00\x13\x00\x23\x00\x02\x00\x04\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x02\x00\x1a\x00\x03\x00\x1a\x00\x1f\x00\x1a\x00\x0f\x00\x10\x00\x11\x00\x12\x00\x13\x00\x02\x00\x05\x00\x0f\x00\x0e\x00\x11\x00\x12\x00\x13\x00\x02\x00\x04\x00\x05\x00\x06\x00\x0c\x00\x0d\x00\x0f\x00\x05\x00\x11\x00\x12\x00\x13\x00\x01\x00\x02\x00\x0f\x00\x23\x00\x11\x00\x12\x00\x13\x00\x23\x00\x09\x00\x0a\x00\x0b\x00\x01\x00\x02\x00\x05\x00\x01\x00\x02\x00\x20\x00\x01\x00\x02\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x09\x00\x0a\x00\x0b\x00\x01\x00\x02\x00\x01\x00\x02\x00\x01\x00\x02\x00\x01\x00\x02\x00\x09\x00\x0a\x00\x09\x00\x0a\x00\x09\x00\x0a\x00\x23\x00\x0a\x00\x19\x00\x1a\x00\x23\x00\x1c\x00\x1d\x00\x05\x00\x06\x00\x20\x00\x21\x00\x07\x00\x08\x00\x0c\x00\x0d\x00\x07\x00\x08\x00\x0c\x00\x0d\x00\x23\x00\x05\x00\x23\x00\x0d\x00\x23\x00\x20\x00\x23\x00\x20\x00\x01\x00\x23\x00\x0c\x00\x01\x00\x07\x00\x0b\x00\x21\x00\x09\x00\x03\x00\x02\x00\x02\x00\x02\x00\x02\x00\x12\x00\x15\x00\x13\x00\x10\x00\x0d\x00\x0f\x00\x0d\x00\x02\x00\x07\x00\x02\x00\x22\x00\x07\x00\x0c\x00\x05\x00\x01\x00\x1b\x00\x11\x00\x0f\x00\x14\x00\x0d\x00\x09\x00\x05\x00\x01\x00\x01\x00\x0f\x00\x0f\x00\x0c\x00\x03\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x64\x00\x2e\x00\x6d\x00\x81\x00\x6f\x00\x1a\x00\x1b\x00\x48\x00\x49\x00\x51\x00\x52\x00\x1a\x00\x1b\x00\x2e\x00\x1a\x00\x1b\x00\x4f\x00\x4e\x00\x6d\x00\xff\xff\xff\xff\xff\xff\x2f\x00\x30\x00\x37\x00\x1f\x00\x20\x00\x2e\x00\x21\x00\x22\x00\x1c\x00\x1d\x00\x23\x00\x24\x00\x2f\x00\x30\x00\x24\x00\x1f\x00\x20\x00\x75\x00\x21\x00\x22\x00\x1a\x00\x1b\x00\x23\x00\x24\x00\x4f\x00\x54\x00\x23\x00\x24\x00\x6e\x00\x1f\x00\x20\x00\x47\x00\x21\x00\x22\x00\x1a\x00\x1b\x00\x23\x00\x24\x00\x1a\x00\x1b\x00\x1a\x00\x1b\x00\x37\x00\x8c\x00\xff\xff\x1c\x00\x7d\x00\x25\x00\x26\x00\x58\x00\x24\x00\x59\x00\x1a\x00\x5a\x00\x25\x00\x26\x00\x1a\x00\x64\x00\x3a\x00\x1c\x00\x79\x00\x25\x00\x26\x00\x1c\x00\x78\x00\x7f\x00\x34\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x35\x00\x2c\x00\x87\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x35\x00\x2c\x00\x37\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x2b\x00\x2c\x00\x25\x00\x26\x00\x42\x00\x72\x00\x78\x00\x3e\x00\x25\x00\x26\x00\x3b\x00\x38\x00\x3c\x00\x77\x00\x25\x00\x26\x00\x24\x00\xff\xff\x43\x00\x6d\x00\x25\x00\x26\x00\x27\x00\x28\x00\x29\x00\x2a\x00\x7e\x00\x2c\x00\x33\x00\x28\x00\x29\x00\x2a\x00\x37\x00\x2c\x00\x6f\x00\x28\x00\x29\x00\x2a\x00\x75\x00\x2c\x00\x9f\x00\x28\x00\x29\x00\x2a\x00\x42\x00\x2c\x00\x25\x00\x26\x00\x38\x00\x39\x00\x74\x00\x25\x00\x26\x00\x55\x00\x56\x00\x25\x00\x26\x00\xff\xff\x66\x00\x25\x00\x26\x00\x73\x00\x25\x00\x26\x00\x6f\x00\x25\x00\x26\x00\x32\x00\x29\x00\x2a\x00\x66\x00\x2c\x00\x83\x00\x29\x00\x2a\x00\xab\xff\x2c\x00\x31\x00\x2a\x00\x71\x00\x2c\x00\x82\x00\x2a\x00\x64\x00\x2c\x00\x30\x00\x37\x00\x2c\x00\x81\x00\x37\x00\x2c\x00\x1a\x00\x1b\x00\x1a\x00\x1b\x00\x1a\x00\x1b\x00\x37\x00\x6e\x00\x3e\x00\x3f\x00\x40\x00\x38\x00\x3c\x00\xff\xff\x38\x00\x85\x00\xff\xff\x37\x00\x6c\x00\x3e\x00\x86\x00\x40\x00\x38\x00\x3c\x00\x37\x00\x7c\x00\x6b\x00\x7b\x00\x1a\x00\x9c\x00\x3e\x00\xa0\x00\x40\x00\x38\x00\x3c\x00\x37\x00\x6a\x00\x41\x00\x69\x00\x40\x00\x38\x00\x3c\x00\x37\x00\x5b\x00\x55\x00\x5c\x00\x44\x00\x45\x00\x91\x00\x68\x00\x40\x00\x38\x00\x3c\x00\x48\x00\x49\x00\xab\x00\xff\xff\x40\x00\x38\x00\x3c\x00\xff\xff\x4a\x00\x4b\x00\x4c\x00\x48\x00\x49\x00\x63\x00\x48\x00\x49\x00\x23\x00\x48\x00\x49\x00\x4a\x00\x4b\x00\x8a\x00\x4a\x00\x4b\x00\x96\x00\x4a\x00\x4b\x00\x93\x00\x48\x00\x49\x00\x48\x00\x49\x00\x48\x00\x49\x00\x48\x00\x49\x00\x50\x00\x4b\x00\x64\x00\x4b\x00\x84\x00\x4b\x00\xff\xff\x8c\x00\x1f\x00\x20\x00\xff\xff\x21\x00\x22\x00\x55\x00\x8f\x00\x23\x00\x24\x00\x51\x00\x8d\x00\x44\x00\x88\x00\x51\x00\xa2\x00\x44\x00\xaa\x00\xff\xff\x61\x00\xff\xff\x5f\x00\xff\xff\x23\x00\xff\xff\x23\x00\x93\x00\xff\xff\x91\x00\x8f\x00\x54\x00\x8a\x00\x24\x00\x47\x00\x7b\x00\x9e\x00\x9c\x00\x9b\x00\x9a\x00\x6f\x00\x99\x00\x6e\x00\x64\x00\x98\x00\x95\x00\x96\x00\xa4\x00\x54\x00\xa2\x00\xa8\x00\x54\x00\xa5\x00\xa9\x00\x61\x00\x9f\x00\x6d\x00\xa6\x00\xad\xff\xaa\x00\x47\x00\x5a\x00\x5f\x00\x5d\x00\xae\x00\xad\x00\x47\x00\xa6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (24, 86) [
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86)
	]

happy_n_terms = 36 :: Prelude.Int
happy_n_nonterms = 28 :: Prelude.Int

happyReduce_24 = happySpecReduce_1  0# happyReduction_24
happyReduction_24 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (TL happy_var_1)) -> 
	happyIn27
		 (happy_var_1
	)}

happyReduce_25 = happySpecReduce_1  1# happyReduction_25
happyReduction_25 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Label happy_var_1)) -> 
	happyIn28
		 (RSTT.Syntax.Abs.Label happy_var_1
	)}

happyReduce_26 = happySpecReduce_1  2# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Var happy_var_1)) -> 
	happyIn29
		 (RSTT.Syntax.Abs.Var happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  3# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOutTok happy_x_1 of { (PT _ (T_Line happy_var_1)) -> 
	happyIn30
		 (RSTT.Syntax.Abs.Line happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  4# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut33 happy_x_1 of { (HappyWrap33 happy_var_1) -> 
	happyIn31
		 (RSTT.Syntax.Abs.Program happy_var_1
	)}

happyReduce_29 = happyReduce 6# 5# happyReduction_29
happyReduction_29 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	case happyOut35 happy_x_5 of { (HappyWrap35 happy_var_5) -> 
	happyIn32
		 (RSTT.Syntax.Abs.DeclCube happy_var_2 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_30 = happyReduce 9# 5# happyReduction_30
happyReduction_30 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	case happyOut38 happy_x_4 of { (HappyWrap38 happy_var_4) -> 
	case happyOut40 happy_x_8 of { (HappyWrap40 happy_var_8) -> 
	happyIn32
		 (RSTT.Syntax.Abs.DeclTopePrefix happy_var_2 happy_var_4 happy_var_8
	) `HappyStk` happyRest}}}

happyReduce_31 = happyReduce 4# 5# happyReduction_31
happyReduction_31 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_3 of { (HappyWrap42 happy_var_3) -> 
	happyIn32
		 (RSTT.Syntax.Abs.DeclCommandProve happy_var_3
	) `HappyStk` happyRest}

happyReduce_32 = happySpecReduce_0  6# happyReduction_32
happyReduction_32  =  happyIn33
		 ([]
	)

happyReduce_33 = happySpecReduce_1  6# happyReduction_33
happyReduction_33 happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	happyIn33
		 ((:[]) happy_var_1
	)}

happyReduce_34 = happySpecReduce_3  6# happyReduction_34
happyReduction_34 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	case happyOut33 happy_x_3 of { (HappyWrap33 happy_var_3) -> 
	happyIn33
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_35 = happySpecReduce_2  7# happyReduction_35
happyReduction_35 happy_x_2
	happy_x_1
	 =  case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	happyIn34
		 (RSTT.Syntax.Abs.NullaryPointConDecl happy_var_2
	)}

happyReduce_36 = happyReduce 5# 7# happyReduction_36
happyReduction_36 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	case happyOut38 happy_x_4 of { (HappyWrap38 happy_var_4) -> 
	happyIn34
		 (RSTT.Syntax.Abs.PrefixPointConDecl happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_37 = happySpecReduce_0  8# happyReduction_37
happyReduction_37  =  happyIn35
		 ([]
	)

happyReduce_38 = happySpecReduce_1  8# happyReduction_38
happyReduction_38 happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	happyIn35
		 ((:[]) happy_var_1
	)}

happyReduce_39 = happySpecReduce_3  8# happyReduction_39
happyReduction_39 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut34 happy_x_1 of { (HappyWrap34 happy_var_1) -> 
	case happyOut35 happy_x_3 of { (HappyWrap35 happy_var_3) -> 
	happyIn35
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_40 = happySpecReduce_3  9# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	case happyOut37 happy_x_3 of { (HappyWrap37 happy_var_3) -> 
	happyIn36
		 (RSTT.Syntax.Abs.CubeProduct happy_var_1 happy_var_3
	)}}

happyReduce_41 = happySpecReduce_1  9# happyReduction_41
happyReduction_41 happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	happyIn36
		 (happy_var_1
	)}

happyReduce_42 = happySpecReduce_1  10# happyReduction_42
happyReduction_42 happy_x_1
	 =  happyIn37
		 (RSTT.Syntax.Abs.CubeUnit
	)

happyReduce_43 = happySpecReduce_1  10# happyReduction_43
happyReduction_43 happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	happyIn37
		 (RSTT.Syntax.Abs.CubeCon happy_var_1
	)}

happyReduce_44 = happySpecReduce_1  10# happyReduction_44
happyReduction_44 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn37
		 (RSTT.Syntax.Abs.CubeVar happy_var_1
	)}

happyReduce_45 = happySpecReduce_3  10# happyReduction_45
happyReduction_45 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_2 of { (HappyWrap36 happy_var_2) -> 
	happyIn37
		 (happy_var_2
	)}

happyReduce_46 = happySpecReduce_1  11# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	happyIn38
		 ((:[]) happy_var_1
	)}

happyReduce_47 = happySpecReduce_3  11# happyReduction_47
happyReduction_47 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	case happyOut38 happy_x_3 of { (HappyWrap38 happy_var_3) -> 
	happyIn38
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_48 = happyReduce 9# 12# happyReduction_48
happyReduction_48 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut41 happy_x_2 of { (HappyWrap41 happy_var_2) -> 
	case happyOut43 happy_x_5 of { (HappyWrap43 happy_var_5) -> 
	case happyOut30 happy_x_6 of { (HappyWrap30 happy_var_6) -> 
	case happyOut42 happy_x_8 of { (HappyWrap42 happy_var_8) -> 
	happyIn39
		 (RSTT.Syntax.Abs.TopeRule happy_var_2 happy_var_5 happy_var_6 happy_var_8
	) `HappyStk` happyRest}}}}

happyReduce_49 = happySpecReduce_0  13# happyReduction_49
happyReduction_49  =  happyIn40
		 ([]
	)

happyReduce_50 = happySpecReduce_1  13# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn40
		 ((:[]) happy_var_1
	)}

happyReduce_51 = happySpecReduce_3  13# happyReduction_51
happyReduction_51 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	case happyOut40 happy_x_3 of { (HappyWrap40 happy_var_3) -> 
	happyIn40
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_52 = happySpecReduce_1  14# happyReduction_52
happyReduction_52 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn41
		 (RSTT.Syntax.Abs.RuleName happy_var_1
	)}

happyReduce_53 = happyReduce 5# 15# happyReduction_53
happyReduction_53 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut44 happy_x_1 of { (HappyWrap44 happy_var_1) -> 
	case happyOut47 happy_x_3 of { (HappyWrap47 happy_var_3) -> 
	case happyOut48 happy_x_5 of { (HappyWrap48 happy_var_5) -> 
	happyIn42
		 (RSTT.Syntax.Abs.Sequent happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_54 = happySpecReduce_0  16# happyReduction_54
happyReduction_54  =  happyIn43
		 ([]
	)

happyReduce_55 = happySpecReduce_1  16# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	happyIn43
		 ((:[]) happy_var_1
	)}

happyReduce_56 = happySpecReduce_3  16# happyReduction_56
happyReduction_56 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut43 happy_x_3 of { (HappyWrap43 happy_var_3) -> 
	happyIn43
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_57 = happySpecReduce_1  17# happyReduction_57
happyReduction_57 happy_x_1
	 =  happyIn44
		 (RSTT.Syntax.Abs.CubeContextEmpty
	)

happyReduce_58 = happySpecReduce_1  17# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn44
		 (RSTT.Syntax.Abs.CubeContextNonEmpty happy_var_1
	)}

happyReduce_59 = happySpecReduce_3  18# happyReduction_59
happyReduction_59 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	case happyOut36 happy_x_3 of { (HappyWrap36 happy_var_3) -> 
	happyIn45
		 (RSTT.Syntax.Abs.PointDecl happy_var_1 happy_var_3
	)}}

happyReduce_60 = happySpecReduce_1  19# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn46
		 ((:[]) happy_var_1
	)}

happyReduce_61 = happySpecReduce_3  19# happyReduction_61
happyReduction_61 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	case happyOut46 happy_x_3 of { (HappyWrap46 happy_var_3) -> 
	happyIn46
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_62 = happySpecReduce_1  20# happyReduction_62
happyReduction_62 happy_x_1
	 =  happyIn47
		 (RSTT.Syntax.Abs.TopeContextEmpty
	)

happyReduce_63 = happySpecReduce_1  20# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn47
		 (RSTT.Syntax.Abs.TopeContextNonEmpty happy_var_1
	)}

happyReduce_64 = happySpecReduce_1  21# happyReduction_64
happyReduction_64 happy_x_1
	 =  happyIn48
		 (RSTT.Syntax.Abs.TopeTop
	)

happyReduce_65 = happySpecReduce_1  21# happyReduction_65
happyReduction_65 happy_x_1
	 =  happyIn48
		 (RSTT.Syntax.Abs.TopeBottom
	)

happyReduce_66 = happySpecReduce_3  21# happyReduction_66
happyReduction_66 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut49 happy_x_3 of { (HappyWrap49 happy_var_3) -> 
	happyIn48
		 (RSTT.Syntax.Abs.TopeImplies happy_var_1 happy_var_3
	)}}

happyReduce_67 = happySpecReduce_1  21# happyReduction_67
happyReduction_67 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn48
		 (happy_var_1
	)}

happyReduce_68 = happySpecReduce_3  22# happyReduction_68
happyReduction_68 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	case happyOut50 happy_x_3 of { (HappyWrap50 happy_var_3) -> 
	happyIn49
		 (RSTT.Syntax.Abs.TopeOr happy_var_1 happy_var_3
	)}}

happyReduce_69 = happySpecReduce_1  22# happyReduction_69
happyReduction_69 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_70 = happySpecReduce_3  23# happyReduction_70
happyReduction_70 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	case happyOut51 happy_x_3 of { (HappyWrap51 happy_var_3) -> 
	happyIn50
		 (RSTT.Syntax.Abs.TopeAnd happy_var_1 happy_var_3
	)}}

happyReduce_71 = happySpecReduce_1  23# happyReduction_71
happyReduction_71 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_72 = happySpecReduce_3  24# happyReduction_72
happyReduction_72 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn51
		 (RSTT.Syntax.Abs.TopeEQ happy_var_1 happy_var_3
	)}}

happyReduce_73 = happyReduce 4# 24# happyReduction_73
happyReduction_73 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn51
		 (RSTT.Syntax.Abs.TopeCon happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_74 = happySpecReduce_1  24# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn51
		 (RSTT.Syntax.Abs.TopeVar happy_var_1
	)}

happyReduce_75 = happySpecReduce_3  24# happyReduction_75
happyReduction_75 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_2 of { (HappyWrap48 happy_var_2) -> 
	happyIn51
		 (happy_var_2
	)}

happyReduce_76 = happySpecReduce_1  25# happyReduction_76
happyReduction_76 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn52
		 ((:[]) happy_var_1
	)}

happyReduce_77 = happySpecReduce_3  25# happyReduction_77
happyReduction_77 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut52 happy_x_3 of { (HappyWrap52 happy_var_3) -> 
	happyIn52
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_78 = happySpecReduce_1  26# happyReduction_78
happyReduction_78 happy_x_1
	 =  happyIn53
		 (RSTT.Syntax.Abs.PointUnit
	)

happyReduce_79 = happyReduce 5# 26# happyReduction_79
happyReduction_79 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	case happyOut53 happy_x_4 of { (HappyWrap53 happy_var_4) -> 
	happyIn53
		 (RSTT.Syntax.Abs.PointPair happy_var_2 happy_var_4
	) `HappyStk` happyRest}}

happyReduce_80 = happyReduce 4# 26# happyReduction_80
happyReduction_80 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn53
		 (RSTT.Syntax.Abs.PointFirst happy_var_3
	) `HappyStk` happyRest}

happyReduce_81 = happyReduce 4# 26# happyReduction_81
happyReduction_81 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn53
		 (RSTT.Syntax.Abs.PointSecond happy_var_3
	) `HappyStk` happyRest}

happyReduce_82 = happyReduce 4# 26# happyReduction_82
happyReduction_82 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn53
		 (RSTT.Syntax.Abs.PointCon happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_83 = happySpecReduce_1  26# happyReduction_83
happyReduction_83 happy_x_1
	 =  case happyOut28 happy_x_1 of { (HappyWrap28 happy_var_1) -> 
	happyIn53
		 (RSTT.Syntax.Abs.nullaryPoint happy_var_1
	)}

happyReduce_84 = happySpecReduce_1  26# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut29 happy_x_1 of { (HappyWrap29 happy_var_1) -> 
	happyIn53
		 (RSTT.Syntax.Abs.PointVar happy_var_1
	)}

happyReduce_85 = happySpecReduce_1  27# happyReduction_85
happyReduction_85 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn54
		 ((:[]) happy_var_1
	)}

happyReduce_86 = happySpecReduce_3  27# happyReduction_86
happyReduction_86 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn54
		 ((:) happy_var_1 happy_var_3
	)}}

happyNewToken action sts stk [] =
	happyDoAction 35# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TL happy_dollar_dollar) -> cont 31#;
	PT _ (T_Label happy_dollar_dollar) -> cont 32#;
	PT _ (T_Var happy_dollar_dollar) -> cont 33#;
	PT _ (T_Line happy_dollar_dollar) -> cont 34#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 35# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap31 x') = happyOut31 x} in x'))

pDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 1# tks) (\x -> happyReturn (let {(HappyWrap32 x') = happyOut32 x} in x'))

pListDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 2# tks) (\x -> happyReturn (let {(HappyWrap33 x') = happyOut33 x} in x'))

pPointConDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 3# tks) (\x -> happyReturn (let {(HappyWrap34 x') = happyOut34 x} in x'))

pListPointConDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 4# tks) (\x -> happyReturn (let {(HappyWrap35 x') = happyOut35 x} in x'))

pCube tks = happySomeParser where
 happySomeParser = happyThen (happyParse 5# tks) (\x -> happyReturn (let {(HappyWrap36 x') = happyOut36 x} in x'))

pCube1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 6# tks) (\x -> happyReturn (let {(HappyWrap37 x') = happyOut37 x} in x'))

pListCube tks = happySomeParser where
 happySomeParser = happyThen (happyParse 7# tks) (\x -> happyReturn (let {(HappyWrap38 x') = happyOut38 x} in x'))

pTopeRule tks = happySomeParser where
 happySomeParser = happyThen (happyParse 8# tks) (\x -> happyReturn (let {(HappyWrap39 x') = happyOut39 x} in x'))

pListTopeRule tks = happySomeParser where
 happySomeParser = happyThen (happyParse 9# tks) (\x -> happyReturn (let {(HappyWrap40 x') = happyOut40 x} in x'))

pRuleName tks = happySomeParser where
 happySomeParser = happyThen (happyParse 10# tks) (\x -> happyReturn (let {(HappyWrap41 x') = happyOut41 x} in x'))

pSequent tks = happySomeParser where
 happySomeParser = happyThen (happyParse 11# tks) (\x -> happyReturn (let {(HappyWrap42 x') = happyOut42 x} in x'))

pListSequent tks = happySomeParser where
 happySomeParser = happyThen (happyParse 12# tks) (\x -> happyReturn (let {(HappyWrap43 x') = happyOut43 x} in x'))

pCubeContext tks = happySomeParser where
 happySomeParser = happyThen (happyParse 13# tks) (\x -> happyReturn (let {(HappyWrap44 x') = happyOut44 x} in x'))

pPointDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 14# tks) (\x -> happyReturn (let {(HappyWrap45 x') = happyOut45 x} in x'))

pListPointDecl tks = happySomeParser where
 happySomeParser = happyThen (happyParse 15# tks) (\x -> happyReturn (let {(HappyWrap46 x') = happyOut46 x} in x'))

pTopeContext tks = happySomeParser where
 happySomeParser = happyThen (happyParse 16# tks) (\x -> happyReturn (let {(HappyWrap47 x') = happyOut47 x} in x'))

pTope tks = happySomeParser where
 happySomeParser = happyThen (happyParse 17# tks) (\x -> happyReturn (let {(HappyWrap48 x') = happyOut48 x} in x'))

pTope1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 18# tks) (\x -> happyReturn (let {(HappyWrap49 x') = happyOut49 x} in x'))

pTope2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 19# tks) (\x -> happyReturn (let {(HappyWrap50 x') = happyOut50 x} in x'))

pTope3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse 20# tks) (\x -> happyReturn (let {(HappyWrap51 x') = happyOut51 x} in x'))

pListTope tks = happySomeParser where
 happySomeParser = happyThen (happyParse 21# tks) (\x -> happyReturn (let {(HappyWrap52 x') = happyOut52 x} in x'))

pPoint tks = happySomeParser where
 happySomeParser = happyThen (happyParse 22# tks) (\x -> happyReturn (let {(HappyWrap53 x') = happyOut53 x} in x'))

pListPoint tks = happySomeParser where
 happySomeParser = happyThen (happyParse 23# tks) (\x -> happyReturn (let {(HappyWrap54 x') = happyOut54 x} in x'))

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Prelude.Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Prelude.Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Prelude.Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Prelude.Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else Prelude.False
         action
          | check     = indexShortOffAddr happyTable off_i
          | Prelude.otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `Prelude.mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
