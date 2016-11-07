import qualified Data.Map as M
import Control.Monad.Reader
data Expr = Const Integer
  | Var String
  | Sum Expr Expr
  | Mul Expr Expr
  | Assign String Integer Expr
  deriving (Show)

eval :: Expr -> Reader (M.Map String Integer) (Maybe Integer)
eval (Const n) = return $ Just n
eval (Var v)   = ask >>= lift . M.lookup v --return $ maybe Nothing id (M.lookup v ask)
--eval (Sum left right) = liftM2 (+) (eval left) (eval right)
eval (Sum x y) =  eval x >>= (<$> eval y) . liftM2 (+)
--eval (Assign name val expr) = return $ local (M.insert name val) (eval expr)
