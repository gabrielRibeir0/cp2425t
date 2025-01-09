import Cp 
import St 


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
outExpr(T op l) = i2 (i2 (op,l))


baseExpr g h f = g -|- (h -|- id >< map f)


recExpr f = baseExpr id id  f

cataExpr g = g . recExpr (cataExpr g) . outExpr

anaExpr f = inExpr . recExpr (anaExpr f) . f

hyloExpr h g = cataExpr h . anaExpr g 

instance Functor (Expr b) where
    fmap f (V x) = V (f x)
    fmap _ (N n) = N n
    fmap f (T op exprs) = T op (map (fmap f) exprs)

instance Applicative (Expr b) where
    pure = return
    (<*>) = aap 

instance Monad (Expr b) where
    return = V
    (V x) >>= f = f x
    (N n) >>= _ = N n
    (T op exprs) >>= f = T op (map (>>= f) exprs)

--catamorfismo monadico
mcataExpr :: Monad m ⇒ (a + (b + (Op, m [c])) → m c) → Expr b a → m c
mcataExpr g = 


-- Maps: Monad: Let Expressions:
--let_exp :: (Num c) ⇒ (a → Expr c b) → Expr c a → Expr c b
--let_exp 



--avaliacao de expressoes
--evaluate :: (Num a, Ord a) ⇒ Expr a b → Maybe a
--evaluate







