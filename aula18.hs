import Data.Char
import Control.Applicative
import Control.Monad

-- type Parser :: String -> Tree
-- type Parser :: String -> (Tree, String)
-- type Parser :: String -> [(Tree, String)]

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\input ->
    case input of
        []      -> []
        c:cs    -> [(c,cs)])

instance Functor Parser where
    -- fmap :: (a->b) -> Parser a -> Parser b
    fmap g px = P (\input ->
        [(g x, resto) | (x, resto) <- parse px input])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P (\input -> [(x, input)])

    -- (<*>) :: Parser (a->b) -> Parser a -> Parser b
    pg <*> px = P (\input -> do
        (g, resto)  <- parse pg input
        (x, resto') <- parse px resto
        return (g x, resto'))

-- pura
a_c :: Parser (Char, Char)
a_c = pure g <*> item <*> item <*> item
    where
        g a b c = (a, c)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    px >>= g = P (\input -> do
        (x, resto) <- parse px input
        parse (g x) resto )

a_c' :: Parser (Char, Char)
a_c' = do
    a <- item
    _ <- item
    c <- item
    return (a,c)

-- :info Alternative
instance Alternative Parser where
    empty :: Parser a
    empty = P (\input -> [])

    (<|>) :: Parser a -> Parser a -> Parser a
    px <|> py = P (\input ->
        (parse px input) ++ (parse py input))
