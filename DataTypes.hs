module DataTypes where

data Frag = Frag [Decl] (Maybe DevDecl) 
             deriving (Show)

data Decl = AssignDecl [Variable] [Expresion] | LlamadaFuncionDecl LlamadaFuncion | LabelDecl String |
            BreakDecl | GotoDecl String | DoDecl Frag | WhileDecl Expresion Frag |
            RepeatDecl Frag Expresion | 
            IfDecl Expresion Frag [(Expresion, Frag)] (Maybe Frag) |
            ForDecl String Expresion Expresion (Maybe Expresion) Frag |
            ForInDecl [String] [Expresion] Frag |
            FnDecDecl [String] (Maybe String) CuerpoFunc |
            LocalFnDecl String CuerpoFunc | LocalVariableDecl [String] [Expresion]
            deriving (Show)


data DevDecl = DevDecl [Expresion] deriving (Show)

data Variable = NameVariable String [TablaBusqueda] | 
           LlamadaFuncionVariable LlamadaFuncion [TablaBusqueda] |
           ExpresionVariable Expresion [TablaBusqueda]
           deriving (Show)

data Expresion = NilExpresion | FalseExpresion | TrueExpresion | NumberExpresion Double | StringExpresion String | 
           VariableArgExpresion | FuncExpresion CuerpoFunc | NameExpresion String [TablaBusqueda] | 
           LlamadaFuncionExpresion LlamadaFuncion [TablaBusqueda] | TblCtorExpresion [Campo] | 
           BinExpresion OperadorBinario Expresion Expresion | OperadorUnarioExpresion OperadorUnario Expresion  
           deriving (Show)

data TablaBusqueda = NameTablaBusqueda String | ExpresionTablaBusqueda Expresion
                 deriving (Show)

data LlamadaFuncion = NameLlamadaFuncion String [LlamadaFuncionArgs] |
              ExpresionLlamadaFuncion Expresion [LlamadaFuncionArgs]
              deriving (Show)

data LlamadaFuncionArgs = LlamadaFuncionArgs [TablaBusqueda] Argumentos | 
                  TblLlamadaFuncionArgs [TablaBusqueda] String Argumentos 
                  deriving (Show)

data Argumentos = ExpresionArgs [Expresion] | TblCtorArgs [Campo] | StrArgs String
            deriving (Show)

data CuerpoFunc = CuerpoFunc ListaParametros Frag deriving (Show)

-- Lista de nombres de parámetros y si hay un número variable de argumentos
data ListaParametros = ListaParametros [String] Bool deriving (Show)

data Campo = ExpresionCampo Expresion | ExpresionExpresionCampo Expresion Expresion | NameExpresionCampo String Expresion
             deriving (Show)

data OperadorBinario = AddOperadorBinario | SubOperadorBinario | MultOperadorBinario | DivOperadorBinario | ExpresionOperadorBinario | ModOperadorBinario | 
             ConcatOperadorBinario | LTOperadorBinario | LTEOperadorBinario | GTOperadorBinario | GTEOperadorBinario |
             EqOperadorBinario | NEqOperadorBinario | AndOperadorBinario | OrOperadorBinario
             deriving (Show)

data OperadorUnario = NegateOperadorUnario | NotOperadorUnario | HashOperadorUnario
            deriving (Show)
