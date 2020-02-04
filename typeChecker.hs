import IntLambda
import Data.Map


type Env = Map Name Type

typeOf :: Env -> Exp -> Type

typeOf env expr = case expr of
	(EInt a)  -> TInt
	(EVar x) -> env ! x
	(ELam x t expr1) -> (t :-> (typeOf env1 expr1))
		where env1 = insert x t env
	(EApp exp1 exp2) -> case (typeOf env exp1) of
		TInt -> error "cannot call EInt"
		(type1 :-> type2) -> if type1 == type0 then type2 else error "not matching"
			where type0 = (typeOf env exp2)