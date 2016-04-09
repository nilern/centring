import System.Environment
import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad
import Control.Monad.State
import Text.Show.Pretty (ppShow)

-- General

data Symbol = Symbol (Maybe String) String
            deriving Show

getCounter :: State Int Int
getCounter = state $ \c -> (c, c)

gensym :: String -> State Int Symbol
gensym s = do c <- get
              put (c + 1)
              return $ Symbol Nothing (s ++ show c)

-- Read into Sexpr

data Sexpr = Int Int
           | Id Symbol
           | List [Sexpr]
           deriving Show

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

-- TODO: gensym, alpha conversion, error handling
-- maybe CPS could be made a monad just to get do-notation?

data CPS = Def Symbol CPS CPS
         | If CPS CPS CPS
         | Fix [(Symbol, [Symbol], [Symbol], CPS)] CPS
         | App CPS [CPS]
         | Primop String [CPS] [Symbol] CPS
         | Const Sexpr
         | Var Symbol
         deriving Show

cps :: Sexpr -> (CPS -> CPS) -> CPS
cps sexp @ (Int _) k = k (Const sexp)
cps (Id sym) k = k (Var sym)
cps (List (Id (Symbol (Just "centring.sf") name):args)) k =
  cpsSpecial name args k
cps (List (Id (Symbol (Just "centring.intr") name):args)) k =
  cpsIntrinsic name args k
cps (List (callee:args)) k = cps callee
  (\f ->
    let r = Symbol Nothing "r"
        v = Symbol Nothing "v"
        any = Symbol (Just "centring.lang") "Any" in
    Fix [(r, [v], [any], k (Var v))]
    (cpsList args (\as -> App f (Var r:as))))

cpsSpecial :: String -> [Sexpr] -> (CPS -> CPS) -> CPS
cpsSpecial "def" [(Id (name @ (Symbol Nothing _))), expr] k = cps expr
  (\v -> Def name v (cpsIntrinsic "make-void" [] k))
cpsSpecial "if" [cond, conseq, alt] c =
  cps cond (\cast ->
             let k = Symbol Nothing "k"
                 v = Symbol Nothing "v"
                 appCont v = App (Var k) [v]
                 any = Symbol (Just "centring.lang") "Any" in
             Fix [(k, [v], [any], c (Var v))]
             (If cast (cps conseq appCont) (cps alt appCont)))
cpsSpecial "letfn" [List defns, body] c =
  Fix (map cpsDefn defns) (cps body c)
  where cpsDefn (List [(Id name), (List formals), (List types), body]) =
          let r = Symbol Nothing "r"
              any = Symbol (Just "centring.lang") "Any"
              retCont v = App (Var r) [v]
              unwrapSym (Id sym) = sym in
          (name, r:map unwrapSym formals, any:map unwrapSym types,
           cps body retCont)
cpsSpecial "fn" args c = cps (giveName args) c
  where giveName args = List [Id $ Symbol (Just "centring.sf") "letfn",
                              List [List (f:args)], f]
          where unwrapSym (Id sym) = sym
                f = Id $ Symbol Nothing "f"
cpsSpecial "do" stmts c = cps (monadize stmts) c
  where monadize [] = List [Id $ Symbol (Just "centring.intr") "make-void"]
        monadize [stmt] = stmt
        monadize (stmt:stmts) =
          List [(List [(Id $ Symbol (Just "centring.fn") "fn"),
                       (List [Id usc]), (List [Id any]), monadize stmts]),
                stmt]
          where usc = Symbol Nothing "_"
                any = Symbol (Just "centring.lang") "Any"

cpsIntrinsic :: String -> [Sexpr] -> (CPS -> CPS) -> CPS
cpsIntrinsic name args k = cpsList args (\as ->
                                          let v = Symbol Nothing "v" in
                                          Primop name as [v] $ k (Var v))

cpsList :: [Sexpr] -> ([CPS] -> CPS) -> CPS
cpsList args k = cpsl args []
  where cpsl (arg:args) res = cps arg (\v -> cpsl args (v:res))
        cpsl [] res = k $ reverse res

-- Work with CPS

mapCps :: (CPS -> CPS) -> CPS -> CPS
mapCps f (Def name v k) = Def name (f v) (f k)
mapCps f (If cond conseq alt) = If (f cond) (f conseq) (f alt)
mapCps f (Fix defns body) = Fix mappedDefns (f body)
  where mappedDefns = map (\(n, fs, ts, b) -> (n, fs, ts, (f b))) defns
mapCps f (App callee args) = App (f callee) (map f args)
mapCps f (Primop name args ress k) = Primop name (map f args) ress (f k)
mapCps f (c @ (Const _)) = f c
mapCps f (v @ (Var _)) = f v

walk :: (CPS -> CPS) -> (CPS -> CPS) -> CPS -> CPS
walk inner outer cexp = outer $ mapCps inner cexp

postWalk :: (CPS -> CPS) -> CPS -> CPS
postWalk f cexp = walk (postWalk f) f cexp

preWalk :: (CPS -> CPS) -> CPS -> CPS
preWalk f cexp = walk (preWalk f) id (f cexp)

foldLeaves :: (a -> CPS -> a) -> a -> CPS -> a
foldLeaves f acc (Def name v k) = let acc' = foldLeaves f acc v in
  foldLeaves f acc' k
foldLeaves f acc (If cond conseq alt) = let acc' = foldLeaves f acc cond
                                            acc'' = foldLeaves f acc' conseq in
                                        foldLeaves f acc'' alt
foldLeaves f acc (Fix defns body) =
  let acc' = foldl (\acc (_, _, _, b) -> foldLeaves f acc b) acc defns in
  foldLeaves f acc' body
foldLeaves f acc (App callee args) = let acc' = foldLeaves f acc callee in
  foldl (foldLeaves f) acc' args
foldLeaves f acc (Primop name args ress k) =
  let acc' = foldl (foldLeaves f) acc args in
  foldLeaves f acc' k
foldLeaves f acc (c @ (Const _)) = f acc c
foldLeaves f acc (v @ (Var _)) = f acc v

-- Main

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn $ ppShow $ fmap (\sexp -> cps sexp haltCont)
    $ parse parseExpr "centring" expr
  where haltCont v = App (Var (Symbol (Just "centring.intr") "halt")) [v]
