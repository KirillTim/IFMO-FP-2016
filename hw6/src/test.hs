{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map as M
import Control.Monad.Reader

--
-- A syntax tree type for simple math, with variables
--
data Exp = IntE Int
         | OpE  Op Exp Exp
         | VarE String
         | LetE String Exp Exp

type Op = Int -> Int -> Int

--
-- The interpreter
--
eval (IntE n)       = return n
eval (OpE op e1 e2) = liftM2 op (eval e1) (eval e2)

eval (VarE x)       = do
    env <- ask
    return $ maybe (error "undefined variable") id (M.lookup x env)

eval (LetE x e1 e2) = do
    env <- ask
    v   <- eval e1
    local (M.insert x v) (eval e2)

--
-- Run the interpreter
--
main = print $ runReader (eval test) M.empty

--
-- A simple text expression:
--
--      let x =
--          let y = 5 + 6
--          in y / 5
--      in x * 3
-- 
-- ==>  6
--
test = LetE "x" (LetE "y" (OpE (+) (IntE 5) (IntE 6))
                      (OpE div y (IntE 5)))
                (OpE (*) x (IntE 3))
    where x = VarE "x"
          y = VarE "y"


