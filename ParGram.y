-- This Happy file was machine-generated by the BNF converter
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGram where
import AbsGram
import LexGram
import ErrM

}

%name pProgram Program
-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype {Token}
%token
  '(' { PT _ (TS _ 1) }
  '()' { PT _ (TS _ 2) }
  ')' { PT _ (TS _ 3) }
  ',' { PT _ (TS _ 4) }
  '->' { PT _ (TS _ 5) }
  ';' { PT _ (TS _ 6) }
  'bool' { PT _ (TS _ 7) }
  'false' { PT _ (TS _ 8) }
  'int' { PT _ (TS _ 9) }
  'main' { PT _ (TS _ 10) }
  'string' { PT _ (TS _ 11) }
  'true' { PT _ (TS _ 12) }
  'type_of(' { PT _ (TS _ 13) }
  '{' { PT _ (TS _ 14) }
  '}' { PT _ (TS _ 15) }

L_ident  { PT _ (TV $$) }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }

Program :: { Program }
Program : 'main' Block { AbsGram.Main $2 }
Block :: { Block }
Block : '{' ListStmt '}' { AbsGram.Block (reverse $2) }
ListStmt :: { [Stmt] }
ListStmt : {- empty -} { [] } | ListStmt Stmt { flip (:) $1 $2 }
Stmt :: { Stmt }
Stmt : ';' { AbsGram.Empty_S }
Bool_literal :: { Bool_literal }
Bool_literal : 'true' { AbsGram.True } | 'false' { AbsGram.Fale }
Simple_type :: { Simple_type }
Simple_type : 'int' { AbsGram.Int_T }
            | 'bool' { AbsGram.Bool_T }
            | 'string' { AbsGram.String_T }
Type :: { Type }
Type : Arg_type { AbsGram.TypeArg_type $1 }
     | Func_type { AbsGram.TypeFunc_type $1 }
Arg_type :: { Arg_type }
Arg_type : Simple_type { AbsGram.Arg_typeSimple_type $1 }
         | Tuple_type { AbsGram.Arg_typeTuple_type $1 }
         | 'type_of(' Ident ')' { AbsGram.Arg_type1 $2 }
Tuple_type :: { Tuple_type }
Tuple_type : '(' ListType ')' { AbsGram.Tuple_T $2 }
Func_type :: { Func_type }
Func_type : Func_arg_type '->' Type { AbsGram.Func_type1 $1 $3 }
          | '()' '->' Type { AbsGram.Func_type2 $3 }
Func_arg_type :: { Func_arg_type }
Func_arg_type : Arg_type { AbsGram.Func_arg_typeArg_type $1 }
              | '(' Func_type ')' { AbsGram.Func_arg_type1 $2 }
ListType :: { [Type] }
ListType : {- empty -} { [] }
         | Type { (:[]) $1 }
         | Type ',' ListType { (:) $1 $3 }
{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    t:_ -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
}

