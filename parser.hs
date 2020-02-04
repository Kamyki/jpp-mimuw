import Prelude hiding (lookup)

import System.IO
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Map



type Var = String
type Loc = Int
type Env = Map Var Loc
type St = Map Loc Int 
type Shell a = (ReaderT Env (StateT St Maybe)) a

data Exp = EInt Int
     | EOp  Op Exp Exp
     | EVar Var

data Op = OpAdd | OpMul | OpSub

data BExp = TT
    | FF
    | BEOp BOp Exp Exp
    | BAnd BExp BExp
    | BOr BExp BExp

data BOp = OpL | OpLeq | OpEq

data Stmt = Skip 
    | Attr Var Exp 
    | Semicolon Stmt Stmt
    | If BExp Stmt Stmt 
    | While BExp Stmt
    | Block [Decl] Stmt 

data Decl = Variable Var Exp


evalBExp :: BExp -> Env -> St -> Bool
evalBExp TT env st = True
evalBExp FF env st = False
evalBExp (BEOp op e1 e2) env st =  
    let v1 = evalExp e1 env st in
    let v2 = evalExp e2 env st in
    case op of
        OpL -> v1 < v2
        OpLeq -> v1 <= v2
        OpEq -> v1 == v2

evalBExp_ :: BExp -> Shell Bool
evalBExp_ TT = return True
evalBExp_ FF = return False
evalBExp_ (BEOp op e1 e2) = do
    v1 <- evalExp_ e1 
    v2 <- evalExp_ e2
    return $ case op of
        OpL -> v1 < v2
        OpLeq -> v1 <= v2
        OpEq -> v1 == v2

        

evalExp :: Exp -> Env -> St -> Maybe Int
evalExp e env st = evalStateT (runReaderT (evalExp_ e) env) st

getLoc :: Var -> Shell Loc
getLoc var = ReaderT (\r -> (StateT (\s ->  lookup var r >>= \a -> Just (a, s))))

getVal :: Loc -> Shell Int
getVal loc = ReaderT (\r -> (StateT (\s -> lookup loc s >>= \a -> Just (a, s))))

evalExp_ :: Exp -> Shell Int
evalExp_ (EVar var) = do 
    loc <- getLoc var
    val <- getVal loc
    return val
evalExp_ (EInt x) = return x
evalExp_ (EOp op e1 e2) = do 
     v1 <- evalExp_ e1
     v2 <- evalExp_ e2
     return $ case op of
          OpAdd -> v1 + v2
          OpSub -> v1 - v2
          OpMul -> v1 * v2


alloc :: St -> Loc
alloc m = size m


execStmt :: Stmt -> IO ()
execStmt stmt = do 
    a <- return $ execStateT (runReaderT (execStmt_ stmt) empty) empty
    print a


execStmt_ :: Stmt -> Shell Int
execStmt_ (Attr var e) = do
    loc <- getLoc var
    val <- evalExp_ e 
    modify (insert loc val)
    return val
execStmt_ (Semicolon s1 s2) = execStmt_ s1 >> execStmt_ s2
execStmt_ (If bx s1 s2) = evalBExp_ bx >>= \b -> case b of
    True -> execStmt_ s1
    False -> execStmt_ s2

execStmt_ (While bx s1) = evalBExp_ bx >>= \b -> case b of
    True -> execStmt_ s1 >> execStmt_ (While bx s1)
    False -> return 0 -- smutne

execStmt_ (Block [] s1) = execStmt_ s1
  
execStmt_ (Block ((Variable var e):xs) s1) = do
    loc <- get >>= return . alloc
    val <- evalExp_ e
    modify (insert loc val)
    local (insert var loc) (execStmt_ (Block xs s1))
  

test1 = Block [Variable "x" (EInt 2)] (If (FF) (Attr "x" (EInt 1)) (Attr "x" (EInt 4)))

test = Block [Variable "x" (EInt 2),
              Variable "y" (EOp OpMul (EVar "x") (EInt 2))] 
    (Semicolon (Attr "x" (EInt 3)) 
               (While (BEOp OpL (EInt 0) (EVar "x"))
                        (Semicolon (Attr "x" (EOp OpSub (EVar "x") (EInt 1))) 
                                    (Attr "y" (EOp OpMul (EVar "y") (EInt 2))))))
        










