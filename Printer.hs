module Printer where

import Language
import PrinterUtility

-- Converts a CoreProgram into a String
pprint :: CoreProgram -> String
pprint = iDisplay . pprProgram

-- Print a CoreProgram
pprProgram :: CoreProgram -> Iseq
pprProgram scdefs = 
    iInterleave sep (map pprScDef scdefs)
    where
    sep = iConcat [ iStr ";", iNewline ]

-- Print a Supercombinator definition
pprScDef :: CoreScDef -> Iseq
pprScDef (name, vars, expr) =
    iConcat [ iStr name, spc, iInterleave (iStr " ") (map iStr vars), iStr " = ", 
              iIndent (pprExpr expr) ]
    where
    spc = if null vars then INil else iStr " "

infixOperators :: [String]
infixOperators = ["|","&","==","~=",">",">=","<","<=","-","+","/","*"]

-- Print an expression
pprExpr :: CoreExpr -> Iseq
-- Expression is a variable
pprExpr (EVar v) = iStr v
-- Expression is a number
pprExpr (ENum n) = iStr (show n)
-- Expression with potentially infix operator
pprExpr (EAp (EAp (EVar op) e1) e2) = 
    if op `elem` infixOperators 
        then iConcat [ pprAExpr e1, iStr " ", iStr op, iStr " ", pprAExpr e2 ]
        else iConcat [ iStr op, iStr " ", pprAExpr e1, iStr " ", pprAExpr e2 ]
-- Application 
pprExpr (EAp e1 e2) = iConcat [ pprExpr e1, iStr " ", pprAExpr e2 ]
-- Let expression
pprExpr (ELet isrec defns expr) =
    iConcat [ iStr keyword, iNewline, iStr "  ", iIndent (pprDefs defns), 
              iNewline, iStr "in ", pprExpr expr ]
    where
    keyword | isrec == Recursive = "letrec"
            | isrec == NonRecursive = "let"
-- Case expression
pprExpr (ECase expr alts) =
    iConcat [ iStr "case ", pprExpr expr, iStr " of", iNewline, iStr "  ",
              iIndent (pprAlts alts) ]
-- Lambda expression
pprExpr (ELam vars expr) =
    iConcat [ iStr "\\ ", iInterleave (iStr " ") (map iStr vars), iStr " . ", 
              pprExpr expr ]
-- Constructor
pprExpr (EConstr t a) = 
    iConcat [ iStr "Pack{", iStr (show t), iStr ",", iStr (show a), iStr "}" ]

-- Print list of definitions in let expressions
pprDefs :: [CoreDef] -> Iseq
pprDefs defns = 
    iInterleave sep (map pprDef defns)
    where
    sep = iConcat [ iStr ";", iNewline ]

-- Print a single definition in let expressions
pprDef :: CoreDef -> Iseq
pprDef (name, expr) = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr) ]

-- Print list of alternatives in case expressions
pprAlts :: [CoreAlt] -> Iseq
pprAlts alts = 
    iInterleave sep (map pprAlt alts)
    where
    sep = iConcat [ iStr ";", iNewline ]

-- Print a single alternative in case expressions
pprAlt :: CoreAlt -> Iseq
pprAlt (n, vars, expr) = 
    iConcat [ iStr "<", iStr (show n), iStr ">", spc, iInterleave (iStr " ") (map IStr vars),
              iStr " -> ", iIndent (pprExpr expr) ]
    where
    spc = if null vars then INil else iStr " "

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

-- Print an expression as atomic expression
pprAExpr :: CoreExpr -> Iseq
pprAExpr e | isAtomicExpr e = pprExpr e
           | otherwise      = iConcat [iStr "(", pprExpr e, iStr ")" ]
