import           Control.Applicative  ((<$>))
import           Control.Monad        (liftM2)
import           Control.Monad.Reader (Reader, ask, local, runReader)
import qualified Data.Map             as M (Map, insert, empty, lookup)
data Expr = Const Integer
  | Var String
  | Sum Expr Expr
  | Mul Expr Expr
  | Assign String Integer Expr
  deriving (Show)

eval :: Expr -> Reader (M.Map String Integer) (Maybe Integer)
eval (Const n)              = return $ Just n
eval (Var v)                = ask >>= \env -> return $ M.lookup v env
eval (Sum x y)              =  eval x >>= (<$> eval y) . liftM2 (+)
eval (Mul x y)              =  eval x >>= (<$> eval y) . liftM2 (*)
eval (Assign name val expr) = local (M.insert name val) (eval expr)

expr1 = Const 10 `Sum` ("x" `Assign` 2 $ Var "x")
--expr2 = "x" `Assign` 2 $ (Var "x" `Mul` ("y" `Assign` 10 $ Var "y"))
--expr3 = Var "x" `Mul` Const 10

--runReader (eval expr1) M.empty
main :: IO ()
main = do putStrLn . show $ runReader (eval expr1) M.empty
