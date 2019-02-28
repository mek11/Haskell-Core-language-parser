module Language where

-- -------------------------------- --
-- DATA TYPES FOR THE CORE LANGUAGE --
-- -------------------------------- --

data Expr a
    = EVar Name             -- Variables
    | ENum Int              -- Numbers
    | EConstr Int Int       -- Constructor tag arity
    | EAp (Expr a) (Expr a) -- Applications
    | ELet                  -- Let(rec) expressions
        IsRec                   -- boolean with True = recursive,
        [Def a]                 -- Definitions
        (Expr a)                -- Body of let(rec)
    | ECase                 -- Case expression
        (Expr a)                -- Expression to scrutinise
        [Alter a]               -- Alternatives
    | ELam [a] (Expr a)     -- Lambda abstractions
    deriving Show

type Name = String

type Program a = [ScDef a]
type ScDef a = (Name, [a], Expr a)
type Def a = (a, Expr a) -- for let and letrec
type Alter a = (Int, [a], Expr a) -- for case
data IsRec = NonRecursive | Recursive
    deriving (Show, Eq)

type CoreProgram = Program Name
type CoreScDef = ScDef Name
type CoreExpr = Expr Name
type CoreAlt = Alter Name
type CoreDef = Def Name