module Types where

data BinOp
    = Add
    | Sub
    | Div
    | Mul
    | LAnd
    | LOr
    | LEq
    deriving (Show, Eq)

data UnOp
    = LNot
    deriving (Show, Eq)

data Var
    = Var Identifier
    | VarOffset Identifier Exp
    | VarDot Identifier Var
    deriving (Show, Eq)

data Exp
    = BinExp Exp BinOp Exp
    | UnExp Exp UnOp
    | AppExp String [Exp]
    | VarExp Var
    | AbsExp Exp
    | ConstExp Integer
    | BoolExp Bool
    | NullExp
    deriving (Show, Eq)

data Stmt
    = RetStmt Exp
    | WriteStmt Exp
    | AllocStmt Var Exp
    | AssignStmt Var Exp
    | IfStmt Exp Stmt (Maybe Stmt)
    | WhileStmt Exp Stmt
    | StmtList [Stmt]
    deriving (Show, Eq)

data Decl
    = VarDecl Identifier Identifier
    | VarDecls [Decl]
    | TypeDecl Identifier Identifier
    | FunDecl Identifier [Decl] Body Type
    deriving (Show, Eq)

data Body = Body [Decl] [Stmt]
    deriving (Show, Eq)

data Identifier
    = TypeId Type
    | VarId String
    deriving (Show, Eq)

data Type
    = SimpleType String
    | ArrayType Type
    | RecordType [Decl]
    deriving (Show, Eq)
