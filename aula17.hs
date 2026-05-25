-- aulas passadas: functor, applicatives, pure, fish
-- monad

data Expr = Val Integer | Div Expr Expr
    deriving (Eq, Show)

safediv :: Integer -> Integer -> Maybe Integer
safediv n 0 = Nothing
safediv n m = Just $ n `div` m

eval_explode :: Expr -> Integer
eval_explode (Val n) = n
eval_explode (Div e1 e2) = (eval_explode e1) `div` (eval_explode e2)

eval_maybe :: Expr -> Maybe Integer
eval_maybe (Val n)      = pure n
eval_maybe (Div e1 e2)  = 
    case eval_maybe e1 of
        Nothing -> Nothing
        Just n1 ->
            case eval_maybe e2 of 
                Nothing -> Nothing
                Just n2 -> 
                    n1 `safediv` n2

-- applicative não resolve, pois ele precisaria que fosse int -> int -> int
-- e o safediv retorna maybe int
-- eval_app :: Expr -> Maybe Integer
-- eval_app (Val n)      = pure n
-- eval_app (Div e1 e2)  = pure safediv <*> eval e1 <*> eval e2

(>>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
mx >>>= g = 
    case mx of
        Nothing -> Nothing
        Just x  -> g x

eval' :: Expr -> Maybe Integer
eval' (Val n)      = pure n
eval' (Div e1 e2)  =
    eval' e1 >>>= (\n1 ->
        eval' e2 >>>= (\n2 ->
            safediv n1 n2
        )
    )

eval_do :: Expr -> Maybe Integer
eval_do (Val n)      = pure n
eval_do (Div e1 e2)  = do
    n1 <- eval_do e1
    n2 <- eval_do e2
    safediv n1 n2

-- monad é um caso particular de aplpicative
-- return = pure
