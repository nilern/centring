import System.Environment
import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Text.Show.Pretty (ppShow)

-- General

data Symbol = Symbol String
            | ModQSymbol String String
            deriving (Show, Eq, Ord)

type GensymState a = State Int a

getCounter :: GensymState Int
getCounter = state $ \c -> (c, c)

gensym :: String -> GensymState Symbol
gensym s = do c <- get
              put (c + 1)
              return $ Symbol (s ++ show c)

ctrAny :: Symbol
ctrAny = ModQSymbol "centring.lang" "Any"

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
                 return $ Id (case modname of
                   Just md -> ModQSymbol md name
                   Nothing -> Symbol name)

parseModname :: Parser String
parseModname = do head <- letter
                  tail <- many $ letter <|> char '.'
                  return $ head:tail

parseName :: Parser String
parseName = do head <- letter <|> oneOf ".+-*/_"
               tail <- many $ alphaNum <|> oneOf ".+-*/_"
               return $ head:tail

-- CPS transform

-- TODO: gensym, alpha conversion, error handling

data CPS = Def Symbol CPS CPS
         | If CPS CPS CPS
         | Fix [(Symbol, [Symbol], [Symbol], CPS)] CPS
         | App CPS [CPS]
         | Primop String [CPS] [Symbol] CPS
         | Const Sexpr
         | Var Symbol
         deriving Show

cps :: Sexpr -> (GensymState CPS -> GensymState CPS) -> GensymState CPS
cps sexp @ (Int _) k = k $ return (Const sexp)
cps (Id sym) k = k $ return (Var sym)
cps (List (Id (ModQSymbol "centring.sf" name):args)) k =
  cpsSpecial name args k
cps (List (Id (ModQSymbol "centring.intr" name):args)) k =
  cpsIntrinsic name args k
cps (List (callee:args)) k = cps callee
  (\mf ->
    do f <- mf
       r <- gensym "r"
       v <-gensym "v"
       defnBody <- k $ return (Var v)
       body <- cpsList args $ liftM (\as -> App f (Var r:as))
       return $ Fix [(r, [v], [ctrAny], defnBody)] body)

cpsSpecial :: String -> [Sexpr] -> (GensymState CPS -> GensymState CPS)
           -> GensymState CPS
cpsSpecial "def" [(Id (name @ (Symbol _))), expr] k = cps expr
  (\mv -> do v <- mv
             cont <- cpsIntrinsic "make-void" [] k
             return $ Def name v cont)
cpsSpecial "if" [cond, conseq, alt] c =
  cps cond (\mcast -> do cast <- mcast
                         k <- gensym "k"
                         v <- gensym "v"
                         let appCont = liftM $ \v -> App (Var k) [v]
                         conseq <- cps conseq appCont
                         alt <- cps alt appCont
                         defnBody <- c $ return (Var v)
                         let body = If cast conseq alt
                         return $ Fix [(k, [v], [ctrAny], defnBody)] body)
cpsSpecial "letfn" [List defns, body] c =
  do defns <- cpsDefns defns []
     body <- cps body c
     return $ Fix defns body
  where cpsDefns (defn:defns) res =
          let (List [(Id name), (List formals), (List types), body]) = defn in
          do r <- gensym "r"
             let retCont = liftM $ \v -> App (Var r) [v]
                 unwrapSym (Id sym) = sym
             body <- cps body retCont
             cpsDefns defns ((name, r:map unwrapSym formals,
                              ctrAny:map unwrapSym types, body):res)
        cpsDefns [] res = return $ reverse res
cpsSpecial "fn" args c = do f <- fmap Id $ gensym "f"
                            let giveName =
                                  List [Id $ ModQSymbol "centring.sf" "letfn",
                                        List [List (f:args)], f]
                            cps giveName c
cpsSpecial "do" stmts c = do sexp <- monadize stmts
                             cps sexp c
  where monadize [] = return $ List [Id $ ModQSymbol "centring.intr" "make-void"]
        monadize [stmt] = return stmt
        monadize (stmt:stmts) =
          do usc <- gensym "_"
             stmts <- monadize stmts
             return $ List [(List [(Id $ ModQSymbol "centring.sf" "fn"),
                                   (List [Id usc]), (List [Id ctrAny]), stmts]),
                            stmt]

cpsIntrinsic :: String -> [Sexpr] -> (GensymState CPS -> GensymState CPS)
             -> GensymState CPS
cpsIntrinsic name args k =
  cpsList args (\mas ->
                 do as <- mas
                    v <- gensym "v"
                    cont <- k $ return (Var v)
                    return $ Primop name as [v] cont)

cpsList :: [Sexpr] -> (GensymState [CPS] -> GensymState CPS) -> GensymState CPS
cpsList args k = cpsl args []
  where cpsl (arg:args) res = cps arg (\mv -> do v <- mv
                                                 cpsl args (v:res))
        cpsl [] res = k $ return (reverse res)

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
walk inner outer cexp = (outer . mapCps inner) cexp

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

-- Utility Passes

countUses :: [Symbol] -> CPS -> [Int]
countUses varNames cexp = foldLeaves uses (fmap (const 0) varNames) cexp
  where uses acc (Const _) = acc
        uses acc (Var name) = fmap updateCount (zip acc varNames)
          where updateCount (count, varName)
                  | varName == name = count + 1
                  | otherwise = count

replaceUse :: Map Symbol CPS -> CPS -> CPS
replaceUse replacements (node @ (Var name)) =
  fromMaybe node (Map.lookup name replacements)
replaceUse _ node = node

-- Main

main :: IO ()
main = do (expr:_) <- getArgs
          let haltCont = liftM $ \v ->
                App (Var (ModQSymbol "centring.intr" "halt")) [v]
              sexpRes = parse parseExpr "centring" expr
              cexpRes = fmap (\sexp -> evalState (cps sexp haltCont) 0) sexpRes
          putStrLn $ ppShow cexpRes
