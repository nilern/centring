import System.Environment
import Text.ParserCombinators.Parsec
import Control.Monad

-- Types

data Sexpr = Int Int
           | Id Symbol
           | List [Sexpr]
           deriving Show

data Symbol = Symbol (Maybe String) String
            deriving Show

data CPS = Const Sexpr
         | Var Symbol
         deriving Show

-- Read

parseExpr :: Parser Sexpr
parseExpr = parseSymbol <|> parseInt <|> parseList

parseList :: Parser Sexpr
parseList = liftM List $ between (char '(') (char ')') (parseExpr `sepBy` spaces)

parseInt :: Parser Sexpr
parseInt = liftM (Int . read) (many1 digit)

parseSymbol :: Parser Sexpr
parseSymbol = do modname <- optionMaybe $ try $
                   do modname <- parseModname
                      char '/'
                      return modname
                 name <- parseName
                 return $ Id $ Symbol modname name

parseModname :: Parser String
parseModname = do head <- letter
                  tail <- many $ letter <|> char '.'
                  return $ head:tail

parseName :: Parser String
parseName = do head <- letter <|> oneOf ".+-*/"
               tail <- many $ alphaNum <|> oneOf ".+-*/"
               return $ head:tail

-- CPS transform

cps :: (CPS -> CPS) -> Sexpr -> CPS
cps k sexp = case sexp of
  Int _ -> k $ Const sexp
  Id sym -> k $ Var sym

-- Main

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ show $ fmap (cps id) $ parse parseExpr "centring" expr
