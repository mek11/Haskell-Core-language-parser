module Parser where

import Control.Applicative
import Data.Char

-- ----------- --
-- PARSER TYPE --
-- ----------- --

newtype Parser a = P (String -> [(a,String)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\inp -> case parse p inp of
                    [] -> []
                    [(v,out)] -> [(f v, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> [(v,inp)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\inp -> case parse pf inp of
                    [] -> []
                    [(f,out)] -> parse (fmap f px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                    [] -> []
                    [(v,out)] -> parse (f v) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\inp -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> case parse p inp of
                    [] -> parse q inp
                    [(v,out)] -> [(v,out)])

-- ------------------- --
-- AUXILIARY FUNCTIONS --
-- ------------------- --

-- Parse a string with the specified parser
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- Returns a parser which succeed only when the specified predicate evaluates to true
sat :: Parser a -> (a -> Bool) -> Parser a
sat p f = do x <- p
             if f x then return x else empty 

-- Returns a parser that ignores spaces before and after applying the specified parser
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

-- Returns a parser which looks for one or more occurrence of something, separated by a separator
someSeparated :: Parser a -> Parser b -> Parser [a]
someSeparated par sep = do v <- par
                           do sep
                              vs <- someSeparated par sep
                              return (v:vs)
                              <|> return [v]

-- ------------- --
-- BASIC PARSERS --
-- ------------- --

-- Parser for a single character
item :: Parser Char
item = P(\inp -> case inp of
            [] -> []
            (x:xs) -> [(x,xs)])

-- Parser for a single digit character
digit :: Parser Char
digit = sat item isDigit

-- Parser for a single lower-case alphabetic character
lower :: Parser Char
lower = sat item isLower

-- Parser for a single upper-case alphabetic character
upper :: Parser Char
upper = sat item isUpper

-- Parser for a single alphabetic character
letter :: Parser Char
letter = sat item isAlpha

-- Parser for a single alphabetic or numeric digit character
alphanum :: Parser Char
alphanum = sat item isAlphaNum

-- Parser for a single alphabetic, numeric digit character or undescrore
alphanum_ :: Parser Char
alphanum_ = alphanum <|> char '_'  

-- Parser for the specified character
char :: Char -> Parser Char
char x = sat item (== x)

-- Parser for the specified string
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- Parser for an identifier
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum_
           return (x:xs)

-- Parser for a natural number
nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

-- Parser for an integer number
int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
         <|> nat

-- Parser for whitespace characters, which ignores them
space :: Parser ()
space = do many (sat item isSpace)
           return ()

-- ------- --
-- PARSERS --
-- ------- --

-- Parser for an identifier
identifier :: Parser String
identifier = token ident

-- Parser for a natural number
natural :: Parser Int
natural = token nat

-- Parser for an integer number
integer :: Parser Int
integer = token int

-- Parser for a specified string
symbol :: String -> Parser String
symbol xs = token (string xs)


