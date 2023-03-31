-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module RSTT.Syntax.Par
  ( happyError
  , myLexer
  , pProgram
  , pDecl
  , pListDecl
  , pShape
  , pPointPattern
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

}

%name pProgram Program
%name pDecl Decl
%name pListDecl ListDecl
%name pShape Shape
%name pPointPattern PointPattern
%name pPointConDecl PointConDecl
%name pListPointConDecl ListPointConDecl
%name pCube Cube
%name pCube1 Cube1
%name pListCube ListCube
%name pTopeRule TopeRule
%name pListTopeRule ListTopeRule
%name pRuleName RuleName
%name pSequent Sequent
%name pListSequent ListSequent
%name pCubeContext CubeContext
%name pPointDecl PointDecl
%name pListPointDecl ListPointDecl
%name pTopeContext TopeContext
%name pTope Tope
%name pTope1 Tope1
%name pTope2 Tope2
%name pTope3 Tope3
%name pListTope ListTope
%name pPoint Point
%name pListPoint ListPoint
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '('      { PT _ (TS _ 1)     }
  ')'      { PT _ (TS _ 2)     }
  ','      { PT _ (TS _ 3)     }
  ':'      { PT _ (TS _ 4)     }
  ':='     { PT _ (TS _ 5)     }
  ';'      { PT _ (TS _ 6)     }
  'cube'   { PT _ (TS _ 7)     }
  'latex'  { PT _ (TS _ 8)     }
  'point'  { PT _ (TS _ 9)     }
  'prove'  { PT _ (TS _ 10)    }
  'render' { PT _ (TS _ 11)    }
  'rule'   { PT _ (TS _ 12)    }
  'shape'  { PT _ (TS _ 13)    }
  'tope'   { PT _ (TS _ 14)    }
  'where'  { PT _ (TS _ 15)    }
  'with'   { PT _ (TS _ 16)    }
  '{'      { PT _ (TS _ 17)    }
  '|'      { PT _ (TS _ 18)    }
  '}'      { PT _ (TS _ 19)    }
  '×'      { PT _ (TS _ 20)    }
  'π₁'     { PT _ (TS _ 21)    }
  'π₂'     { PT _ (TS _ 22)    }
  '⇒'      { PT _ (TS _ 23)    }
  '∧'      { PT _ (TS _ 24)    }
  '∨'      { PT _ (TS _ 25)    }
  '≡'      { PT _ (TS _ 26)    }
  '⊢'      { PT _ (TS _ 27)    }
  '⊤'      { PT _ (TS _ 28)    }
  '⊥'      { PT _ (TS _ 29)    }
  '⋅'      { PT _ (TS _ 30)    }
  '⋆'      { PT _ (TS _ 31)    }
  '⟨'      { PT _ (TS _ 32)    }
  '⟩'      { PT _ (TS _ 33)    }
  '𝟙'      { PT _ (TS _ 34)    }
  L_quoted { PT _ (TL $$)      }
  L_Label  { PT _ (T_Label $$) }
  L_Var    { PT _ (T_Var $$)   }
  L_Line   { PT _ (T_Line $$)  }

%%

String  :: { String }
String   : L_quoted { $1 }

Label :: { RSTT.Syntax.Abs.Label }
Label  : L_Label { RSTT.Syntax.Abs.Label $1 }

Var :: { RSTT.Syntax.Abs.Var }
Var  : L_Var { RSTT.Syntax.Abs.Var $1 }

Line :: { RSTT.Syntax.Abs.Line }
Line  : L_Line { RSTT.Syntax.Abs.Line $1 }

Program :: { RSTT.Syntax.Abs.Program }
Program : ListDecl { RSTT.Syntax.Abs.Program $1 }

Decl :: { RSTT.Syntax.Abs.Decl }
Decl
  : 'cube' Label 'with' '{' ListPointConDecl '}' { RSTT.Syntax.Abs.DeclCube $2 $5 }
  | 'tope' Label '(' ListCube ')' 'with' '{' ListTopeRule '}' { RSTT.Syntax.Abs.DeclTopePrefix $2 $4 $8 }
  | 'shape' Var ':=' Shape { RSTT.Syntax.Abs.DeclShape $2 $4 }
  | 'prove' '{' Sequent '}' { RSTT.Syntax.Abs.DeclCommandProve $3 }
  | 'render' 'latex' Shape { RSTT.Syntax.Abs.DeclCommandRenderLatex $3 }

ListDecl :: { [RSTT.Syntax.Abs.Decl] }
ListDecl
  : {- empty -} { [] }
  | Decl { (:[]) $1 }
  | Decl ';' ListDecl { (:) $1 $3 }

Shape :: { RSTT.Syntax.Abs.Shape }
Shape
  : '{' PointPattern ':' Cube '|' Tope '}' { RSTT.Syntax.Abs.Shape $2 $4 $6 }

PointPattern :: { RSTT.Syntax.Abs.PointPattern }
PointPattern
  : Var { RSTT.Syntax.Abs.PointPatternVar $1 }
  | '⟨' PointPattern ',' PointPattern '⟩' { RSTT.Syntax.Abs.PointPatternPair $2 $4 }

PointConDecl :: { RSTT.Syntax.Abs.PointConDecl }
PointConDecl
  : 'point' Label { RSTT.Syntax.Abs.NullaryPointConDecl $2 }
  | 'point' Label '(' ListCube ')' { RSTT.Syntax.Abs.PrefixPointConDecl $2 $4 }

ListPointConDecl :: { [RSTT.Syntax.Abs.PointConDecl] }
ListPointConDecl
  : {- empty -} { [] }
  | PointConDecl { (:[]) $1 }
  | PointConDecl ';' ListPointConDecl { (:) $1 $3 }

Cube :: { RSTT.Syntax.Abs.Cube }
Cube
  : Cube '×' Cube1 { RSTT.Syntax.Abs.CubeProduct $1 $3 }
  | Cube1 { $1 }

Cube1 :: { RSTT.Syntax.Abs.Cube }
Cube1
  : '𝟙' { RSTT.Syntax.Abs.CubeUnit }
  | Label { RSTT.Syntax.Abs.CubeCon $1 }
  | Var { RSTT.Syntax.Abs.CubeVar $1 }
  | '(' Cube ')' { $2 }

ListCube :: { [RSTT.Syntax.Abs.Cube] }
ListCube : Cube { (:[]) $1 } | Cube ',' ListCube { (:) $1 $3 }

TopeRule :: { RSTT.Syntax.Abs.TopeRule }
TopeRule
  : 'rule' RuleName 'where' '{' ListSequent Line ';' Sequent '}' { RSTT.Syntax.Abs.TopeRule $2 $5 $6 $8 }

ListTopeRule :: { [RSTT.Syntax.Abs.TopeRule] }
ListTopeRule
  : {- empty -} { [] }
  | TopeRule { (:[]) $1 }
  | TopeRule ';' ListTopeRule { (:) $1 $3 }

RuleName :: { RSTT.Syntax.Abs.RuleName }
RuleName : String { RSTT.Syntax.Abs.RuleName $1 }

Sequent :: { RSTT.Syntax.Abs.Sequent }
Sequent
  : CubeContext '|' TopeContext '⊢' Tope { RSTT.Syntax.Abs.Sequent $1 $3 $5 }

ListSequent :: { [RSTT.Syntax.Abs.Sequent] }
ListSequent
  : {- empty -} { [] }
  | Sequent { (:[]) $1 }
  | Sequent ';' ListSequent { (:) $1 $3 }

CubeContext :: { RSTT.Syntax.Abs.CubeContext }
CubeContext
  : '⋅' { RSTT.Syntax.Abs.CubeContextEmpty }
  | ListPointDecl { RSTT.Syntax.Abs.CubeContextNonEmpty $1 }

PointDecl :: { RSTT.Syntax.Abs.PointDecl }
PointDecl : Var ':' Cube { RSTT.Syntax.Abs.PointDecl $1 $3 }

ListPointDecl :: { [RSTT.Syntax.Abs.PointDecl] }
ListPointDecl
  : PointDecl { (:[]) $1 }
  | PointDecl ',' ListPointDecl { (:) $1 $3 }

TopeContext :: { RSTT.Syntax.Abs.TopeContext }
TopeContext
  : '⋅' { RSTT.Syntax.Abs.TopeContextEmpty }
  | ListTope { RSTT.Syntax.Abs.TopeContextNonEmpty $1 }

Tope :: { RSTT.Syntax.Abs.Tope }
Tope
  : '⊤' { RSTT.Syntax.Abs.TopeTop }
  | '⊥' { RSTT.Syntax.Abs.TopeBottom }
  | Tope '⇒' Tope1 { RSTT.Syntax.Abs.TopeImplies $1 $3 }
  | Tope1 { $1 }

Tope1 :: { RSTT.Syntax.Abs.Tope }
Tope1
  : Tope1 '∨' Tope2 { RSTT.Syntax.Abs.TopeOr $1 $3 } | Tope2 { $1 }

Tope2 :: { RSTT.Syntax.Abs.Tope }
Tope2
  : Tope2 '∧' Tope3 { RSTT.Syntax.Abs.TopeAnd $1 $3 } | Tope3 { $1 }

Tope3 :: { RSTT.Syntax.Abs.Tope }
Tope3
  : Point '≡' Point { RSTT.Syntax.Abs.TopeEQ $1 $3 }
  | Label '(' ListPoint ')' { RSTT.Syntax.Abs.TopeCon $1 $3 }
  | Var { RSTT.Syntax.Abs.TopeVar $1 }
  | '(' Tope ')' { $2 }

ListTope :: { [RSTT.Syntax.Abs.Tope] }
ListTope : Tope { (:[]) $1 } | Tope ',' ListTope { (:) $1 $3 }

Point :: { RSTT.Syntax.Abs.Point }
Point
  : '⋆' { RSTT.Syntax.Abs.PointUnit }
  | '⟨' Point ',' Point '⟩' { RSTT.Syntax.Abs.PointPair $2 $4 }
  | 'π₁' '(' Point ')' { RSTT.Syntax.Abs.PointFirst $3 }
  | 'π₂' '(' Point ')' { RSTT.Syntax.Abs.PointSecond $3 }
  | Label '(' ListPoint ')' { RSTT.Syntax.Abs.PointCon $1 $3 }
  | Label { RSTT.Syntax.Abs.nullaryPoint $1 }
  | Var { RSTT.Syntax.Abs.PointVar $1 }

ListPoint :: { [RSTT.Syntax.Abs.Point] }
ListPoint : Point { (:[]) $1 } | Point ',' ListPoint { (:) $1 $3 }

{

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

}

