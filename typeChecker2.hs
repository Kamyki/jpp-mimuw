import qualified Data.Map as Map
import Data.Maybe(fromJust)
import Control.Monad.Reader
import Control.Monad.Except
import IntLambda


type Env = Map.Map Name Type
type TCM a = ExceptT String (Reader Env) a

{-
ExceptT e m a 
e - error type
m - inner monad 
a - value type


runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT :: TCM a -> Reader Env (Either String a)

MonadReader r m => MonadReader r (ExceptT e m)
Monad m => MonadError e (ExceptT e m)

throwError :: e -> m a
catchError :: m a -> (e -> m a) -> m a
-}

typeOf :: Exp -> TCM Type
typeOf (EInt a) = return TInt
typeOf (EVar x) = do
	env <- ask
	case (env Map.!? x) of
		Just t -> return t
		Nothing -> throwError ("unknown variable " ++ x)

typeOf (EApp exp1 exp2) = do 
	type1 <- typeOf exp1
	type2 <- typeOf exp2
	case type1 of
		TInt -> throwError "cannot call EInt"
		(type3 :-> type4) -> if type3 == type2
			then return type4
			else throwError "not matching"

typeOf (ELam x t e) = do
	t1 <- local (Map.insert x t) $ typeOf e
	return $ t :-> t1


typeOf1 :: Exp -> TCM Type
typeOf1 exp = catchError (typeOf exp) hadler where
	handler e = throwError $ "Type error in\n" ++ (show exp) ++ "\n" ++ e

typeCheck :: Exp -> IO ()
typeCheck expr = do
	print $ runReader (runExceptT $ typeOf1 expr) Map.empty

