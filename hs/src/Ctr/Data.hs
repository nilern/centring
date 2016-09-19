module Ctr.Data where
import Data.Vector (Vector)
import qualified Data.Map as Map

data AST = Fn Symbol Symbol (Vector (Condition, AST)) SrcPos
         | App AST AST SrcPos
         | Def Symbol AST SrcPos
         | Primop Primop (Vector AST) (Vector AST) SrcPos
         | Closure Env AST SrcPos
         | Do (Vector AST)
         | Var Symbol SrcPos
         | Const Value SrcPos

type Condition = Vector Clause

type Clause = Vector Atom

data Atom = Base AST
          | Not AST

data Primop = Expr String (Vector Value -> Value)
            | Stmt String (Vector Value -> ())
            | Ctrl String (Vector Value -> Vector AST -> AST)

data Value = Int Int
           | Bool Bool
           | Char Char
           | Symbol Symbol

instance Show Value where
  show (Int i) = show i
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Char c) = ['\\', c]
  show (Symbol s) = show s

data Symbol = Litsym String
            | Gensym String Int

instance Show Symbol where
  show (Litsym s) = s
  show (Gensym s i) = s ++ show i

type Env = [Map.Map Symbol Value]

data SrcPos = SrcPos { filename :: String
                     , pos :: Int
                     , line :: Int
                     , col :: Int }
