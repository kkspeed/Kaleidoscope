module Syntax where

type Name = String

data Expr = Float Double
          | Var String
          | Call Name [Expr]
          | Function Name [Name] Expr
          | BinaryOp Name Expr Expr
          | UnaryOp Name Expr
          | Extern Name [Name]
          deriving (Eq, Ord, Show)
