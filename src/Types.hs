module Types where

data BinOp
    = Add
    | Sub
    | Div
    | Mul
    deriving Show

data UnOp
    = LNot
    deriving Show

data Var
    = VarId String
    | VarOffset Var Exp
    | VarDot Var String
    deriving (Show)

data Exp
    = BinExp Exp BinOp Exp
    | UnExp Exp UnOp
    | AppExp String [Exp]
    | VarExp Var
    | AbsExp Exp
    | ConstExp Integer
    | BoolExp Bool
    | NullExp
    deriving Show

data Stmt
    = RetStmt Exp
    | WriteStmt Exp
    | AllocStmt Var Exp
    | AssignStmt Var Exp
    | IfStmt Exp Stmt (Maybe Stmt)
    | WhileStmt Exp Stmt
    deriving Show

data Decl = Decl
    deriving Show

data Body = Body [Decl] [Stmt]
    deriving Show
