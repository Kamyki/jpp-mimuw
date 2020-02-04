module SkelGram where

-- Haskell module generated by the BNF converter

import AbsGram
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Main block -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty_S -> failure x
transBool_literal :: Bool_literal -> Result
transBool_literal x = case x of
  True -> failure x
  Fale -> failure x
transSimple_type :: Simple_type -> Result
transSimple_type x = case x of
  Int_T -> failure x
  Bool_T -> failure x
  String_T -> failure x
transType :: Type -> Result
transType x = case x of
  TypeArg_type argtype -> failure x
  TypeFunc_type functype -> failure x
transArg_type :: Arg_type -> Result
transArg_type x = case x of
  Arg_typeSimple_type simpletype -> failure x
  Arg_typeTuple_type tupletype -> failure x
  Arg_type1 ident -> failure x
transTuple_type :: Tuple_type -> Result
transTuple_type x = case x of
  Tuple_T types -> failure x
transFunc_type :: Func_type -> Result
transFunc_type x = case x of
  Func_type1 funcargtype type_ -> failure x
  Func_type2 type_ -> failure x
transFunc_arg_type :: Func_arg_type -> Result
transFunc_arg_type x = case x of
  Func_arg_typeArg_type argtype -> failure x
  Func_arg_type1 functype -> failure x
