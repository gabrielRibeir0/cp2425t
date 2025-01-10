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
    fmap f (V x) = V (f x)
    fmap _ (N n) = N n
    fmap f (T op l) = T op (map (fmap f) l)

instance Applicative (Expr b) where
    pure = return
    (<*>) = aap

instance Monad (Expr b) where
    return = V
    (V x) >>= f = f x
    (N n) >>= _ = N n
    (T op l) >>= f = T op (map (>>= f) l)

mcataExpr :: Monad m => (Either a ( Either b (Op, m [c])) -> m c) -> Expr b a -> m c
mcataExpr g = g .! (aux . recExpr (mcataExpr g) . outExpr)

aux :: Monad m => Either a (Either b (Op, [m c])) -> m (Either a (Either b (Op, m [c])))
aux (Left a) = return (Left a)
aux (Right (Left b)) = return (Right (Left b))
aux (Right (Right (op, l))) = do
    cs <- sequence l
    return (Right (Right (op, return cs)))


-- Maps: Monad: Let Expressions:
let_exp :: Num c => (a -> Expr c b) -> Expr c a -> Expr c b
let_exp f (V x) = f x 
let_exp _ (N n) = N n
let_exp f (T op l) = T op (map (let_exp f) l) 



--avaliacao de expressoes
evaluate :: (Num a, Ord a) => Expr a b -> Maybe a
evaluate = cataExpr eval 

eval :: (Num a, Ord a) => Either b (Either a (Op, [Maybe a])) -> Maybe a
eval (Left _) = Nothing
eval (Right (Left n)) = Just n
eval (Right (Right (op, vals))) = case (op, sequence vals) of
    (Add, Just [x, y]) -> Just (x + y)  
    (Mul, Just [x, y]) -> Just (x * y)
    (ITE, Just [cond, t, e]) -> if cond > 0 then Just t else Just e
    _ -> Nothing







