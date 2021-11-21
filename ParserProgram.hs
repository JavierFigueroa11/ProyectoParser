module ParserProgram where
import DataTypes
import System.Environment
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error
import qualified Text.ParserCombinators.Parsec.Token as Token
import Data.Maybe (isNothing)

parsearArchivo :: String -> IO Frag
parsearArchivo file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r

parsearString :: String -> Frag
parsearString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

-- Lexical token definitions 
languageDef =
  emptyDef { Token.commentStart    = ""
           , Token.commentEnd      = ""
           , Token.commentLine     = "--"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.opStart         = oneOf "+-*/^%.<>=~#:"
           , Token.opLetter        = oneOf "=:."
           , Token.reservedNames   = ["and", "break", "do", "else", "elseif",
                                      "end", "false", "for", "function",
                                      "goto", "if", "in", "local", "nil", 
                                      "not", "or", "repeat", "return", "then",
                                      "true", "until", "while"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", "^", "%", ".."
                                     ,"<", "<=", ">", ">=", "==", "~="
                                     ,"#", "=", "...", ".", ":"
                                     ]
           }

lexer         = Token.makeTokenParser languageDef
identifier    = Token.identifier      lexer 
reserved      = Token.reserved        lexer 
reservedOp    = Token.reservedOp      lexer 
parens        = Token.parens          lexer 
brackets      = Token.brackets        lexer 
braces        = Token.braces          lexer 
integer       = Token.integer         lexer 
float         = Token.float           lexer 
semi          = Token.semi            lexer 
whiteSpace    = Token.whiteSpace      lexer 
comma         = Token.comma           lexer
stringLiteral = Token.stringLiteral   lexer

-- Parser functions

whileParser :: Parser Frag
whileParser = whiteSpace >> (do { c <- parsearFrag; eof; return c})

parsearNum :: Parser Double
parsearNum = try float <|>
              do { i <- integer; return $ fromIntegral i }

parsearFrag :: Parser Frag
parsearFrag = do
  stats <- (endBy parsearDecl (optional semi))
  retDecl <- optionMaybe parsearDevDecl
  return $ Frag stats retDecl

parsearDecl :: Parser Decl
parsearDecl = try assignDecl <|> fnCallDecl <|> labelDecl <|> breakDecl <|> 
            gotoDecl <|> doDecl <|> whileDecl <|> repeatDecl <|> ifDecl <|>
            (try forDecl) <|> forInDecl <|> funcDecDecl <|> 
            (try localFuncDecl) <|> localVariableDecl
  where
    assignDecl = do 
      vars <- parsearVariableList
      reservedOp "="
      exps <- parsearExpresionList
      return $ AssignDecl vars exps 
    fnCallDecl = do
      f <- parsearLlamadaFuncion
      return $ LlamadaFuncionDecl f
    labelDecl = do
      reservedOp "::"
      id <- identifier
      reservedOp "::"
      return $ LabelDecl id
    breakDecl = do
      reserved "break" 
      return BreakDecl
    gotoDecl = do
      reserved "goto"
      id <- identifier
      return $ GotoDecl id
    doDecl = do
      reserved "do"
      block <- parsearFrag
      reserved "end"
      return $ DoDecl block
    whileDecl = do
      reserved "while"
      exp <- parseExpresion
      reserved "do"
      block <- parsearFrag
      reserved "end"
      return $ WhileDecl exp block
    repeatDecl = do
      reserved "repeat"
      block <- parsearFrag
      reserved "until"
      exp <- parseExpresion
      return $ RepeatDecl block exp
    ifDecl = do
      reserved "if"
      exp <- parseExpresion
      reserved "then"
      block <- parsearFrag
      elifs <- many (do 
        reserved "elseif"
        e <- parseExpresion
        reserved "then"
        b <- parsearFrag
        return (e,b))
      els <- optionMaybe (do {reserved "else"; parsearFrag})
      reserved "end"
      return $ IfDecl exp block elifs els
    forDecl = do
      reserved "for"
      id <- identifier
      reservedOp "="
      exp1 <- parseExpresion
      comma
      exp2 <- parseExpresion
      exp3 <- optionMaybe $ do {comma; parseExpresion}
      reserved "do"
      block <- parsearFrag
      reserved "end"
      return $ ForDecl id exp1 exp2 exp3 block
    forInDecl = do
      reserved "for"
      ids <- parsearNameList
      reserved "in"
      exps <- parsearExpresionList
      reserved "do"
      block <- parsearFrag
      reserved "end"
      return $ ForInDecl ids exps block
    funcDecDecl = do
      reserved "function"
      ids <- sepBy1 identifier (reservedOp ".")
      id2 <- optionMaybe $ do {reservedOp ":"; identifier}
      funcBody <- parsearCuerpoFunc
      return $ FnDecDecl ids id2 funcBody
    localFuncDecl = do
      reserved "local"
      reserved "function"
      id <- identifier
      funcBody <- parsearCuerpoFunc
      return $ LocalFnDecl id funcBody
    localVariableDecl = do
      reserved "local"
      ids <- parsearNameList
      exps <- option [] $ do {reservedOp "="; parsearExpresionList}
      return $ LocalVariableDecl ids exps
   
parsearDevDecl = do
  reserved "return"
  exps <- option [] parsearExpresionList
  option "" semi
  return $ DevDecl exps

parsearVariableList :: Parser [Variable]
parsearVariableList = sepBy1 parsearVariable comma

parsearVariable :: Parser Variable
parsearVariable = try fnCallVariable <|> expVariable <|> nameVariable 
  where
    fnCallVariable = do
      fnCall <- parsearLlamadaFuncion
      tblLookups <- many1 parsearTablaBusqueda
      return $ LlamadaFuncionVariable fnCall tblLookups
    expVariable = do
      exp <- parens parseExpresion
      tblLookups <- many1 parsearTablaBusqueda
      return $ ExpresionVariable exp tblLookups
    nameVariable = do
      id <- identifier
      tblLookups <- many parsearTablaBusqueda
      return $ NameVariable id tblLookups

parsearNameList :: Parser [String]
parsearNameList = sepBy1 identifier comma

parsearExpresionList :: Parser [Expresion]
parsearExpresionList = sepBy1 parseExpresion comma

parseExpresion = buildExpressionParser opTable parseExpresionTerm where
  parseExpresionTerm = parens parseExpresion <|> exp

  opTable = 
    [[Infix  (reservedOp "^" >> return (BinExpresion ExpresionOperadorBinario)) AssocLeft],

     [Prefix (reserved "not" >> return (OperadorUnarioExpresion NotOperadorUnario)),
      Prefix (reservedOp "#" >> return (OperadorUnarioExpresion HashOperadorUnario)),
      Prefix (reservedOp "-" >> return (OperadorUnarioExpresion NegateOperadorUnario))],

     [Infix  (reservedOp "*" >> return (BinExpresion MultOperadorBinario)) AssocLeft,
      Infix  (reservedOp "/" >> return (BinExpresion DivOperadorBinario)) AssocLeft,
      Infix  (reservedOp "%" >> return (BinExpresion ModOperadorBinario)) AssocLeft],

     [Infix  (reservedOp "+" >> return (BinExpresion AddOperadorBinario)) AssocLeft,
      Infix  (reservedOp "-" >> return (BinExpresion SubOperadorBinario)) AssocLeft],

     [Infix  (reservedOp ">" >> return (BinExpresion GTOperadorBinario)) AssocLeft,
      Infix  (reservedOp ">=" >> return (BinExpresion GTEOperadorBinario)) AssocLeft,
      Infix  (reservedOp "<" >> return (BinExpresion LTOperadorBinario)) AssocLeft,
      Infix  (reservedOp "<=" >> return (BinExpresion LTEOperadorBinario)) AssocLeft,
      Infix  (reservedOp "~=" >> return (BinExpresion NEqOperadorBinario)) AssocLeft,
      Infix  (reservedOp "==" >> return (BinExpresion EqOperadorBinario)) AssocLeft],
       
     [Infix  (reserved "and" >> return (BinExpresion AndOperadorBinario)) AssocLeft],

     [Infix  (reserved "or" >> return (BinExpresion OrOperadorBinario)) AssocLeft]
    ]

  exp = nilExpresion <|> falseExpresion <|> trueExpresion <|> strExpresion <|> varArgExpresion <|>
        fnDefExpresion <|> (try fnCallExpresion) <|> nameExpresion <|> 
        tblCtorExpresion <|> numExpresion

  nilExpresion = reserved "nil" >> return NilExpresion
  falseExpresion = reserved "false" >> return FalseExpresion
  trueExpresion = reserved "true" >> return TrueExpresion
  varArgExpresion = reservedOp "..." >> return VariableArgExpresion
  numExpresion = do
    n <- parsearNum
    return $ NumberExpresion n
  strExpresion = do
    s <- stringLiteral
    return $ StringExpresion s
  fnDefExpresion = do
    reserved "function"
    funcBody <- parsearCuerpoFunc
    return $ FuncExpresion funcBody
  fnCallExpresion = do
    f <- parsearLlamadaFuncion
    tblLookups <- many parsearTablaBusqueda
    return $ LlamadaFuncionExpresion f tblLookups
  nameExpresion = do
    id <- identifier
    tblLookups <- many parsearTablaBusqueda
    return $ NameExpresion id tblLookups
  tblCtorExpresion = do
    fieldlist <- parseTblCtor
    return $ TblCtorExpresion fieldlist


parsearTablaBusqueda :: Parser TablaBusqueda
parsearTablaBusqueda = expTablaBusqueda <|> nameTablaBusqueda 
  where
    expTablaBusqueda = do
      exp <- brackets parseExpresion
      return $ ExpresionTablaBusqueda exp
    nameTablaBusqueda = do
      reservedOp "."
      id <- identifier
      return $ NameTablaBusqueda id
  
parsearLlamadaFuncion :: Parser LlamadaFuncion
parsearLlamadaFuncion = nameLlamadaFuncion <|> expLlamadaFuncion 
  where
    nameLlamadaFuncion = do
      id <- identifier
      fnCallArgs <- many1 $ try parsearLlamadaFuncionArgs
      return $ NameLlamadaFuncion id fnCallArgs
    expLlamadaFuncion = do
      exp <- parens parseExpresion
      fnCallArgs <- many1 $ try parsearLlamadaFuncionArgs
      return $ ExpresionLlamadaFuncion exp fnCallArgs

parsearLlamadaFuncionArgs :: Parser LlamadaFuncionArgs
parsearLlamadaFuncionArgs = do
  ts <- many parsearTablaBusqueda
  args ts <|> tblArgs ts
  where
    args ts = do
      a <- parsearArgs
      return $ LlamadaFuncionArgs ts a 
    tblArgs ts = do
      reservedOp ":"
      id <- identifier
      a <- parsearArgs
      return $ TblLlamadaFuncionArgs ts id a   
 
parsearArgs :: Parser Argumentos
parsearArgs = expArgs <|> tblCtorArgs <|> strArgs 
  where
    expArgs = do
      expList <- parens (option [] parsearExpresionList)
      return $ ExpresionArgs expList
    tblCtorArgs = do
      fields <- parseTblCtor
      return $ TblCtorArgs fields
    strArgs = do
      str <- stringLiteral
      return $ StrArgs str

parsearCuerpoFunc :: Parser CuerpoFunc
parsearCuerpoFunc = do
  parlist <- parens (option (ListaParametros [] False) parsearListaParametros)
  block <- parsearFrag
  reserved "end"
  return $ CuerpoFunc parlist block

parsearListaParametros :: Parser ListaParametros
parsearListaParametros = nameListaParametros <|> varArgListaParametros 
  where
    nameListaParametros = do
      id <- identifier
      ids <- many $ try (comma >> identifier)
      let params = id:ids
      varArgStr <- optionMaybe (comma >> reservedOp "...")
      let hasVariableArgs = if isNothing varArgStr then False else True
      return $ ListaParametros params hasVariableArgs
    varArgListaParametros = do
      reservedOp "..." 
      return $ ListaParametros [] True

parseTblCtor :: Parser [Campo]
parseTblCtor = braces $ sepEndBy parsearCampo (comma <|> semi)

parsearCampo :: Parser Campo
parsearCampo = expExpresionCampo <|> (try nameExpresionCampo) <|> expCampo 
  where
    expExpresionCampo = do
      e1 <- brackets parseExpresion
      reservedOp "="
      e2 <- parseExpresion
      return $ ExpresionExpresionCampo e1 e2 
    nameExpresionCampo = do
      i <- identifier
      reservedOp "="
      e <- parseExpresion
      return $ NameExpresionCampo i e 
    expCampo = do
      e <- parseExpresion
      return $ ExpresionCampo e