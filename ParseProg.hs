module ParseProg where 

import Control.Applicative
import Language
import Parser

-- ----------------------------- --
-- PARSERS FOR THE CORE LANGUAGE --
-- ----------------------------- --

-- Parser for a program
parseProg :: Parser CoreProgram
parseProg = someSeparated parseScDef (symbol ";")

-- Parser for a supercombinator
parseScDef :: Parser CoreScDef
parseScDef = do v <- parseVarName 
                pf <- many parseVarName 
                symbol "="
                body <- parseExpr
                return (v, pf, body)

-- Parser for an expression
parseExpr :: Parser CoreExpr
parseExpr = parseLet
        <|> parseLetRec
        <|> parseCase
        <|> parseLambda
        <|> parseExpr1

-- Parser for a variable
parseVar :: Parser CoreExpr
parseVar = fmap EVar parseVarName

-- Parser for a variable name
parseVarName :: Parser String
parseVarName = do v <- sat identifier (`notElem` keywords)
                  return v

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

-- Parser for a number
parseNum :: Parser CoreExpr
parseNum = do n <- integer
              return (ENum n)

-- Parser for a let expression
parseLet :: Parser CoreExpr
parseLet = do symbol "let"
              defs <- parseDefs
              symbol "in"
              expr <- parseExpr
              return (ELet NonRecursive defs expr)

-- Parser for a letrec expression
parseLetRec :: Parser CoreExpr
parseLetRec = do symbol "letrec"
                 defs <- parseDefs
                 symbol "in"
                 expr <- parseExpr
                 return (ELet Recursive defs expr)

-- Parser for a case expression
parseCase :: Parser CoreExpr
parseCase =  do symbol "case"
                expr <- parseExpr
                symbol "of"
                alts <- parseAlts
                return (ECase expr alts)

-- Parse for a lambda expression
parseLambda :: Parser CoreExpr
parseLambda = do symbol "\\ "
                 vars <- some parseVarName 
                 symbol "."
                 expr <- parseExpr
                 return (ELam vars expr)

-- Parser for an atomic expression
parseAExpr :: Parser CoreExpr
parseAExpr = parseVar
             <|> parseNum
             <|> parseConstr
             <|> do symbol "("
                    expr <- parseExpr
                    symbol ")"
                    return expr

-- Parser for definitions in a let or letrec expression
parseDefs :: Parser [CoreDef]
parseDefs = someSeparated parseDef (symbol ";")

-- Parse for a single definition in a let or letrec expression
parseDef :: Parser CoreDef
parseDef = do var <- parseVarName 
              symbol "="
              expr <- parseExpr
              return (var, expr)

-- Parser for alternatives in a case expression
parseAlts :: Parser [CoreAlt]
parseAlts = someSeparated parseAlt (symbol ";")

-- Parser for a single alternative in a case expression
parseAlt :: Parser CoreAlt
parseAlt = do symbol "<"
              n <- natural
              symbol ">"
              vars <- many parseVarName 
              symbol "->"
              expr <- parseExpr
              return (n, vars, expr)

-- Parser for a constructor
parseConstr :: Parser CoreExpr
parseConstr = do symbol "Pack"
                 symbol "{"
                 tag <- natural
                 symbol ","
                 arity <- natural
                 symbol "}"
                 return (EConstr tag arity)

-- Parser which considers "|" operator
parseExpr1 :: Parser CoreExpr
parseExpr1 = do a <- parseExpr2
                do symbol "|"
                   b <- parseExpr1
                   return (EAp (EAp (EVar "|") a) b)
                   <|> return a

-- Parser which considers "&" operator
parseExpr2 :: Parser CoreExpr
parseExpr2 = do a <- parseExpr3
                do symbol "&"
                   b <- parseExpr2
                   return (EAp (EAp (EVar "&") a) b)
                   <|> return a

-- Parser which considers comparison operator
parseExpr3 :: Parser CoreExpr
parseExpr3 = do a <- parseExpr4
                do relOp <- parseRelOp
                   b <- parseExpr4
                   return (EAp (EAp (EVar relOp) a) b)
                   <|> return a

parseRelOp :: Parser String
parseRelOp = symbol "=="
         <|> symbol "~="
         <|> symbol ">="
         <|> symbol ">"
         <|> symbol "<="
         <|> symbol "<"

-- Parser which considers "+" and "-" operators   
parseExpr4 :: Parser CoreExpr
parseExpr4 = do a <- parseExpr5
                do symbol "+"
                   b <- parseExpr4
                   return (EAp (EAp (EVar "+") a) b)
                   <|> do symbol "-"
                          b <- parseExpr5
                          return (EAp (EAp (EVar "-") a) b)
                          <|> return a

-- Parser which considers "*" and "/" operators 
parseExpr5 :: Parser CoreExpr
parseExpr5 = do a <- parseExpr6
                do symbol "*"
                   b <- parseExpr5
                   return (EAp (EAp (EVar "*") a) b)
                   <|> do symbol "/"
                          b <- parseExpr6
                          return (EAp (EAp (EVar "/") a) b)
                          <|> return a

-- Parser which considers application of atomic expressions 
parseExpr6 :: Parser CoreExpr
parseExpr6 = do a <- parseAExpr -- first atomic expression
                as <- many parseAExpr -- following atomic expressions
                return (foldl EAp a as)

-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl :: (ExprN -> ExprN -> ExprN) -> ExprN -> [ExprN] -> ExprN
-- EAp :: Expr a -> Expr a -> Expr a