import Cp 


data Op = ITE 
        | Add 
        | Mul 
        | Suc
        deriving (Show,Eq)


data Expr b a = V a 
                | N b 
                | T Op [Expr b a]
                deriving (Show,Eq)

inExpr = either V (either N (uncurry T))

outExpr(V a) = i1 a
outExpr(N b) = i1 b
outExpr(T op l) = i2(op,l)

baseExpr g h f = g -|- (h -|- id >< map f)

 
recExpr f = baseExpr id id f 

cataExpr g = g . recExpr (cataExpr g) . outExpr

anaExpr f = inExpr . recExpr (anaExpr f) . f

hyloExpr h g = cataExpr h . anaExpr g 

-- Maps: Monad: Let Expressions:
--let_exp :: (Num c) ⇒ (a → Expr c b) → Expr c a → Expr c b
--let_exp 

--catamorfismo monadico
--mcataExpr :: Monad m ⇒ (a + (b + (Op, m [c])) → m c) → Expr b a → m c
--mcataExpr g

--avaliacao de expressoes
--evaluate :: (Num a, Ord a) ⇒ Expr a b → Maybe a
--evaluate












