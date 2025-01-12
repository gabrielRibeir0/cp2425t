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

soma x y = T Add [x, y]
multi x y = T Mul [x, y]
ite x y z = T ITE [x, y, z]

inExpr :: Either a (Either b (Op, [Expr b a])) -> Expr b a
inExpr = either V (either N (uncurry T))

outExpr :: Expr a1 a2 -> Either a2 (Either a1 (Op, [Expr a1 a2]))
outExpr(V a) = i1 a
outExpr(N b) = i2 (i1 b)
outExpr(T op l) = i2 (i2 (op,l))

baseExpr g h f = g -|- (h -|- id >< map f)

recExpr f = baseExpr id id  f

cataExpr g = g . recExpr (cataExpr g) . outExpr

anaExpr f = inExpr . recExpr (anaExpr f) . f

hyloExpr h g = cataExpr h . anaExpr g

instance Functor (Expr b) where
    fmap f = cataExpr (inExpr . baseExpr f id id)

instance Applicative (Expr b) where
    pure = return
    (<*>) = aap

instance Monad (Expr b) where
    return = V
    e >>= f = cataExpr (either f (either N (uncurry T))) e

mcataExpr :: Monad m => (Either a ( Either b (Op, m [c])) -> m c) -> Expr b a -> m c
mcataExpr g = g .! (aux . recExpr (mcataExpr g) . outExpr)

aux :: Monad m => Either a (Either b (Op, [m c])) -> m (Either a (Either b (Op, m [c])))
aux = either (return . i1) (either (return . i2 . i1) ret)
    where ret = return . i2 . i2 . split p1 (sequence . p2)

-- Maps: Monad: Let Expressions:
let_exp :: Num c => (a -> Expr c b) -> Expr c a -> Expr c b
let_exp = flip (>>=)

--avaliacao de expressoes
evaluate :: (Num a, Ord a) => Expr a b -> Maybe a
evaluate = cataExpr eval

eval :: (Num a, Ord a) => Either b (Either a (Op, [Maybe a])) -> Maybe a
eval = either nothing (either Just g)
    where g (op, vals) = case (op, sequence vals) of
            (Add, Just [x, y]) -> Just (x + y)
            (Mul, Just [x, y]) -> Just (x * y)
            (ITE, Just [cond, t, e]) -> if cond > 0 then Just t else Just e
            _ -> Nothing

e = ite (V "x") (N 0) (ite (V "x") (N 0) (multi (V "y") (soma (N 3) (V "y"))))

f :: Num b => String -> Expr b a
f "x" = N 0
f "y" = N 5
f _ = N 99