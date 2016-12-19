module Types where

data BinOp
    = Add
    | Sub
    | Div
    | Mul
    | LAnd
    | LOr
    | LEq
    deriving Show

data UnOp
    = LNot
    deriving Show

data Var
    = Var Identifier
    | VarOffset Identifier Exp
    | VarDot Identifier Var
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

data Decl
    = VarDecl Identifier Identifier
    | VarDecls [Decl]
    | TypeDecl Identifier Identifier
    | FunDecl
    deriving Show

data Body = Body [Decl] [Stmt]
    deriving Show

data Identifier
    = TypeId String
    | VarId String
    deriving Show
