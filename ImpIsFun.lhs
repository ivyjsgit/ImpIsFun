> {-# LANGUAGE GADTSyntax        #-}
>
> import           Parsing2
>
> import qualified Data.Map           as M
> import           Text.Read          (readMaybe)
> import           System.Environment (getArgs)
> import            Data.Tuple

> type Var = String
>
> type Prog = [Stmt]
>
> data Type where
>   TyInt  :: Type
>   TyBool :: Type
>   TyFun :: [Type]-> Type -> Type
>   deriving (Show, Eq)
>
> type FunctionArgs = (Type, Var)

> data Stmt where
>   Function :: Type -> String -> [FunctionArgs] ->  Stmt -> Stmt
>   Decl   :: Type -> Var -> Stmt           -- <type> <var>
>   Assign :: Var  -> Expr -> Stmt          -- <var> ':=' <expr>
>   Block  :: Prog -> Stmt                  -- '{' <prog> '}'
>   If     :: Expr -> Stmt -> Stmt -> Stmt  -- 'if' <expr> 'then' <stmt> 'else' <stmt>
>   Repeat :: Expr -> Stmt -> Stmt          -- 'repeat' <expr> <stmt>
>   While  :: Expr -> Stmt -> Stmt          -- 'while' <expr> <stmt>
>   Input  :: Var  -> Stmt                  -- 'input' <var>
>   Output :: Expr -> Stmt                  -- 'output' <expr>
>   Return :: Expr -> Stmt
>   deriving Show
>
> data Expr where
>   EInt  :: Integer -> Expr                -- <int>
>   EBool :: Bool    -> Expr                -- 'False' | 'True'
>   EArrayInt :: [Integer] -> Expr          -- [Ints]
>   EVar  :: Var -> Expr                    -- <var>
>   EUn   :: UOp -> Expr -> Expr            -- <uop> <expr>
>   EBin  :: BOp -> Expr -> Expr -> Expr    -- <expr> <bop> <expr>
>   EString :: String -> Expr
>   FunctionCall :: String -> [Expr] -> Expr

>   deriving Show
>
> data UOp = Neg | Not | Sqrt | Length
>   deriving (Show, Eq)
>
> data BOp = Add | Sub | Mul | Div | And | Or | Equals | Less | Mod | Concat
>   deriving (Show, Eq)

Parser
------

> lexer :: TokenParser u
> lexer = makeTokenParser $
>   emptyDef
>   { reservedNames   = [ "True", "False", "if", "then", "else", "begin", "end"
>                         , "repeat", "while", "input", "output", "int", "bool", "arrayInt", "function", "string", "function", "call" ]
>   , reservedOpNames = [ "=", "==", "<", "+", "-", "*", "~", "&&", "||", "sqrt", "++", "\"", "length", ","  ]
>   }
>
> parens :: Parser a -> Parser a
> parens = getParens lexer
>
> reserved, reservedOp :: String -> Parser ()
> reserved   = getReserved lexer
> reservedOp = getReservedOp lexer
>
> symbol :: String -> Parser String
> symbol = getSymbol lexer
>
> ident :: Parser String
> ident = getIdentifier lexer
>
> integer :: Parser Integer
> integer = getInteger lexer
>
> whiteSpace :: Parser ()
> whiteSpace = getWhiteSpace lexer

> parseFunction :: Parser Stmt
> parseFunction = Function <$>( (reserved "function") *> parseType) <*> ident <*> parseArgList <*> parseBlock 

> parseArgList :: Parser [FunctionArgs]
> parseArgList = (symbol "(" *> (parseArgs `sepBy` (reservedOp ",")) <* symbol ")")

> parseArgs :: Parser FunctionArgs
> parseArgs = (,) <$> parseType <*> ident

> parseAtom :: Parser Expr
> parseAtom
>   =   EInt        <$> integer
>   <|> EBool True  <$  reserved "True"
>   <|> EBool False <$  reserved "False"
>   <|> EVar        <$> ident
>   <|> (EString <$> (reservedOp "\"" *> ident <* reservedOp "\""))
>   <|> parens parseExpr
>   <|> parseCall

> parseCall:: Parser Expr
> parseCall =  FunctionCall <$ reserved "call" <*> ident <*> parseExprList

> parseExprList :: Parser [Expr]
> parseExprList = (symbol "(" *> (parseExpr `sepBy` (reservedOp ",")) <* symbol ")")


> parseExpr :: Parser Expr
> parseExpr = buildExpressionParser table parseAtom
>   where
>     table = [ [ unary  "~"  (EUn Not) ]
>             , [ unary  "-"  (EUn Neg) ]
>             , [ binary "*"  (EBin Mul)    AssocLeft
>               , binary "/"  (EBin Div)    AssocLeft
>               ,   binary "%"  (EBin Mod)    AssocLeft ]
>             , [ binary "+"  (EBin Add)    AssocLeft
>               , binary "-"  (EBin Sub)    AssocLeft
>               , unary  "sqrt"  (EUn Sqrt)
>               , binary "++"  (EBin Concat)    AssocLeft
>               , unary  "length"  (EUn Length)
>               ]
>             , [ binary "==" (EBin Equals) AssocNone
>               , binary "<"  (EBin Less)   AssocNone
>               ]
>             , [ binary "&&" (EBin And)    AssocRight ]
>             , [ binary "||" (EBin Or)     AssocRight ]
>             ]
>     unary  name fun       = Prefix (fun <$ reservedOp name)
>     binary name fun assoc = Infix  (fun <$ reservedOp name) assoc
>
> parseProg :: Parser Prog
> parseProg = parseStmt `sepBy` (reservedOp ";")
>
> parseStmt :: Parser Stmt
> parseStmt =
>       parseBlock
>   <|> If      <$> (reserved "if" *> parseExpr)
>               <*> (reserved "then" *> parseStmt)
>               <*> (reserved "else" *> parseStmt)
>   <|> Repeat  <$> (reserved "repeat" *> parseExpr) <*> parseBlock
>   <|> While   <$> (reserved "while" *> parseExpr)  <*> parseBlock
>   <|> Input   <$> (reserved "input" *> ident)
>   <|> Output  <$> (reserved "output" *> parseExpr)
>   <|> Return  <$> (reserved "return" *> parseExpr)
>   <|> Assign  <$> ident <*> (reservedOp "=" *> parseExpr)
>   <|> Decl    <$> parseType <*> ident
>   <|> parseFunction
>
> parseType :: Parser Type
> parseType = (TyInt <$ reserved "int") <|> (TyBool <$ reserved "bool")
> parseBlock :: Parser Stmt
> parseBlock = Block  <$> (symbol "{" *> parseProg <* symbol "}")
>
> impParser :: Parser Prog
> impParser = whiteSpace *> parseProg <* eof

Type checking expressions
-------------------------

> data TypeError where
>   DuplicateVar :: Var -> TypeError
>   UndefinedVar :: Var  -> TypeError
>   Mismatch     :: Expr -> Type -> Type -> TypeError
>   MismatchNumArgs :: TypeError
>   InputBool    :: Var  -> TypeError
>   MissingFun :: String -> TypeError
>   NotAFun :: TypeError
>   OutsideScope :: Var -> TypeError
>   Other :: TypeError
>   deriving Show
>
> showTyError :: TypeError -> String
> showTyError (DuplicateVar x) = "Duplicate variable declaration: " ++ x
> showTyError (UndefinedVar x) = "Variable used before declaration: " ++ x
> showTyError (Mismatch e ty1 ty2) =
>   unlines
>     [ "Type mismatch in expression " ++ show e
>     , "  expected " ++ show ty1
>     , "  but got " ++ show ty2 ++ " instead."
>     ]
> showTyError (InputBool x) = "Cannot 'input' a boolean variable."
> showTyError (MismatchNumArgs) = "There is a mismatch in the number of args in your function. Either you called too many or too little args."
> showTyError (Other) = "This error shouldn't appear."
> showTyError (NotAFun) = "Error: You tried to call something that isn't a function"
> showTyError (OutsideScope s) = "Variable outside of scope: " ++ s
> type Ctx = M.Map Var Type
>

> type FunctionsInfer = M.Map String ([Type],Type)


> infer :: Ctx -> Expr -> Either TypeError Type
> infer _   (EInt _)    = Right TyInt   -- Integers have type int
> infer _   (EBool _)    = Right TyBool  -- Booleans have type bool
> infer ctx (EVar x)      =               -- Look up the type of variables
>   case M.lookup x ctx of                  --   in the context
>     Nothing -> Left $ UndefinedVar x
>     Just ty -> Right ty
> infer ctx (EBin op e1 e2)  = inferBin ctx op e1 e2   -- Call helper functions for
> infer ctx (EUn op e)      = inferUn ctx op e        -- binary & unary operators
> infer ctx (FunctionCall name expressions)  = case M.lookup name ctx of
>   Nothing -> Left (MissingFun name)
>   Just (TyFun args returnType) -> (checkArgs expressions args ctx) *> Right returnType
>   Just _ -> Left NotAFun
> checkArgs :: [Expr] -> [Type]-> Ctx -> Either TypeError ()
> checkArgs [] [] _ = Right ()
> checkArgs [] (types) ctx = Left MismatchNumArgs
> checkArgs exprs [] ctx = Left MismatchNumArgs
> checkArgs (e:exprs) (t:types) ctx= (check ctx e t) *> checkArgs exprs types ctx

> binTy :: BOp -> (Type, Type, Type)  -- (input1, input2, output)
> binTy op
>   | op `elem` [Add, Sub, Mul, Div, Mod] = (TyInt, TyInt, TyInt)
>   | op `elem` [And, Or]            = (TyBool, TyBool, TyBool)
>   | op `elem` [Equals, Less]       = (TyInt, TyInt, TyBool)
>   | otherwise                      = error "Unhandled operator in binTy"

> inferBin :: Ctx -> BOp -> Expr -> Expr -> Either TypeError Type
> inferBin ctx op e1 e2 =
>   case binTy op of
>     (ty1, ty2, tyOut) ->
>       check ctx e1 ty1 *>
>       check ctx e2 ty2 *>
>       Right tyOut


> unTy :: UOp -> (Type, Type)
> unTy Neg = (TyInt, TyInt)
> unTy Sqrt = (TyInt, TyInt)
> unTy Not = (TyBool, TyBool)
>
> inferUn :: Ctx -> UOp -> Expr -> Either TypeError Type
> inferUn ctx op e =
>   case unTy op of
>     (tyIn, tyOut) ->
>       check ctx e tyIn *>
>       Right tyOut


> check :: Ctx -> Expr -> Type -> Either TypeError ()
> check ctx e ty =
>   infer ctx e >>= \ty' ->
>   case ty == ty' of
>     False -> Left $ Mismatch e ty ty'
>     True  -> Right ()




> checkProg :: Ctx -> Prog -> Maybe Type -> Either TypeError Ctx
> checkProg ctx []     _ = Right ctx
> checkProg ctx (s:ss) a = (checkStmt ctx s a) >>= \ newContext -> (checkProg newContext ss a)        


> checkStmt :: Ctx -> Stmt -> Maybe Type -> Either TypeError Ctx

> checkStmt ctx (Decl ty x) a = case M.lookup x ctx of
>								Just n -> Left (DuplicateVar x)
>								Nothing -> Right (M.insert x ty ctx)

> checkStmt ctx (Assign x e) a= case M.lookup x ctx of
>								Just n -> (check ctx e n) >>= \ a-> Right (M.insert x n ctx)
>								Nothing -> Left (UndefinedVar x)
> checkStmt ctx (Block ss) a = checkProg ctx ss a *> Right ctx
> checkStmt ctx (If e s1 s2) a =
>   check ctx e TyBool *>
>   checkStmt ctx s1 a *>
>   checkStmt ctx s2 a *>
>   Right ctx
> checkStmt ctx (Repeat e body) a=
>   check ctx e TyInt *>
>   checkStmt ctx body a *>
>   Right ctx
> checkStmt ctx (While e body)  a=
>   check ctx e TyBool *>
>   checkStmt ctx body a *>
>   Right ctx
> checkStmt ctx (Function ourType name args body) a= (checkStmt (tupleToCtx args ctx)) body (Just ourType) *> Right (M.insert name (TyFun (map fst args) ourType) ctx)
> checkStmt ctx (Return expr) a= case a of
>   Nothing -> Left Other
>   Just a -> case check ctx expr a of
>       Left a -> Left (OutsideScope (""++show(expr)))
>       Right a -> Right ctx
> checkStmt ctx (Input v)  a  =
>   case M.lookup v ctx of
>     Nothing    -> Left $ UndefinedVar v
>     Just TyInt -> Right ctx
>     Just _     -> Left $ InputBool v
> checkStmt ctx (Output e)  a =
>   check ctx e TyInt *> Right ctx



> tupleToCtx :: [FunctionArgs] -> Ctx -> Ctx
> tupleToCtx [] a= a
> tupleToCtx ((ourType, var):cons) ctx= tupleToCtx cons (M.insert var ourType ctx)



> type Value = Integer

> type Mem = M.Map Var Value
> type Functions = M.Map String ([Var],Stmt)


> interpExpr :: Mem -> Expr -> World -> Value
> interpExpr _ (EInt i)  _     = i
> interpExpr _ (EBool b)  _    = fromBool b
> interpExpr m (EVar x) _      =
>   case M.lookup x m of
>     Just v  -> v
>     Nothing -> error $ "Impossible! Uninitialized variable " ++ x
> interpExpr m (EBin b e1 e2) w = interpBOp b (interpExpr m e1 w) (interpExpr m e2 w)
> interpExpr m (EUn  u e)    w = interpUOp u (interpExpr m e w)
> interpExpr m (FunctionCall functionName args) world@(W memory input output funs val) = case M.lookup functionName funs of
>   Nothing -> error $ "Function doesn't exist"
>   Just (n,r) -> let newMem = (doArgs args m n world) in case interpStmt r (W newMem input output funs val)of
>       W _ _ _ _ val -> val

> interpArgs :: [Expr] -> Mem -> World -> [Value]
> interpArgs [] m w= []
> interpArgs (e:exprs) m w=  [(interpExpr m e w)] ++ interpArgs exprs m w

> doArgs :: [Expr] -> Mem -> [String] -> World -> Mem
> doArgs a b c w = insertArgs (interpArgs a b w) b c

> insertArgs :: [Value] -> Mem -> [String]-> Mem
> insertArgs [] m [] = m
> insertArgs (a:args) m (n:names) = insertArgs args (M.insert n (a) m) names

> interpUOp :: UOp -> Value -> Value
> interpUOp Neg v = -v
> interpUOp Not v = 1-v
> interpUOp Sqrt v = floor (sqrt (fromIntegral v))
>
> interpBOp :: BOp -> Value -> Value -> Value
> interpBOp Add    = (+)
> interpBOp Sub    = (-)
> interpBOp Mul    = (*)
> interpBOp Mod    = \a b -> mod a b
> interpBOp Div    = div
> interpBOp And    = (*)
> interpBOp Or     = \v1 v2 -> min 1 (v1 + v2)
> interpBOp Equals = \v1 v2 -> fromBool (v1 == v2)
> interpBOp Less   = \v1 v2 -> fromBool (v1 <  v2)
>
> fromBool :: Bool -> Value
> fromBool False = 0
> fromBool True  = 1

> data World where
>   W  :: Mem       -- Current state of memory
>      -> [String]  -- Strings typed by the user, waiting to be read by 'input'
>      -> [String]  -- Strings produced by 'output' (newest first)
>      -> Functions 
>      -> Value
>      -> World
>   Error :: World  -- Something went wrong
>   deriving Show
>

> -- An initial world state, given user input
> initWorld :: String -> World
> initWorld inp = W M.empty (words inp) [] M.empty 0


> interpStmt :: Stmt -> World -> World

> interpStmt ourDecl@(Decl ourType var) world= interpDecl ourDecl world
> interpStmt (Block prog) world=  interpBlock prog world
> interpStmt (If expression statement1 statement2) ourWorld= interpIf expression statement1 statement2 ourWorld
> interpStmt (Repeat expression statement) world@(W memory input output funs val) =  interpRepeat (interpExpr memory expression world) statement world
> interpStmt (While expression statement) world= interpWhile expression statement world
> interpStmt ourInput@(Input ourVar) world= interpInput ourInput  world
> interpStmt ourOutput@(Output ourExpression) world= interpOutput ourOutput  world
> interpStmt (Assign ourVar ourExpression) world@(W memory input output funs val) = let ourVal = (interpExpr memory ourExpression world) in (W (M.insert ourVar ourVal memory) input output funs val)
> interpStmt (Function returnType funName args body) world@(W memory input output funs val) =  (W memory input output (M.insert funName (map snd args, body) funs) val)
> interpStmt (Return expr) world@(W memory input output funs val)= (W memory input output funs (interpExpr memory expr world)) 



> interpRepeat :: Integer -> Stmt -> World -> World
> interpRepeat 0 stmt wrld = wrld 
> interpRepeat intx stmt wrld@(W memory input output funs val) = interpRepeat (intx - 1) stmt (interpStmt stmt wrld)

> interpDecl :: Stmt -> World -> World
> interpDecl  (Decl ourType var) world@(W memory input output funs val) = (W (M.insert var 0 memory) input output funs val)
> interpBlock :: Prog -> World -> World
> interpBlock prog world = interpProg prog world

> interpIf :: Expr -> Stmt -> Stmt -> World -> World
> interpIf ourExpression firstStmt secondStmt ourWorld@(W memory input output funs val) = 
>  if (interpExpr memory ourExpression ourWorld) > 0 
>	then interpStmt firstStmt ourWorld
> 	else interpStmt secondStmt ourWorld

> interpWhile :: Expr -> Stmt -> World -> World
> interpWhile ourExpression ourStmt ourWorld@(W memory input output funs val) = if (interpExpr memory ourExpression ourWorld) == 0 
> then ourWorld
> else  interpWhile ourExpression ourStmt (interpStmt ourStmt ourWorld)

> interpInput :: Stmt -> World -> World 
> interpInput (Input ourNum) world@(W memory (ourInput:input) output funs val) = case readMaybe ourInput of
> 					Nothing -> Error
>					Just num -> (W (M.insert ourNum num memory) input output funs val)

	 

> interpOutput :: Stmt -> World -> World 
> interpOutput (Output ourNum) world@(W memory input output funs val) =  (W memory input ((show(interpExpr memory ourNum world)):output) funs val)

> interpProg :: Prog -> World -> World
> interpProg [] world = world
> interpProg (ourExpression:ourProg) ourWorld = interpProg ourProg (interpStmt ourExpression ourWorld)

> formatWorld :: World -> String
> formatWorld (W m _ o funs val) = unlines $
>      reverse o
>   ++ ["-----"]
>   ++ map formatVar (M.assocs m)
> formatWorld Error = "Error"
>
> formatVar (x,v) = x ++ " -> " ++ show v
>
> run :: String -> IO ()
> run fileName = do
>   s <- readFile fileName
>   case parse impParser s of
>     Left err -> print err
>     Right p  ->
>       case checkProg M.empty p Nothing of
>         Left tyErr -> putStrLn (showTyError tyErr)
>         Right _    -> do
>           inp <- getContents
>           let es = interpProg p (initWorld inp)
>           putStr $ formatWorld es
>
> main :: IO ()
> main = do
>   args <- getArgs
>   case args of
>     []     -> putStrLn "Please provide a file name."
>     (fn:_) -> run fn
