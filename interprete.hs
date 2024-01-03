import System.Environment
import System.IO
import Data.Char
import Control.Monad

-- Instrucciones: ADD, MOV, CMP, BNC, BEQ, PrintR, PrintS...
-- Etiquetas: etiqueta:

---- cConstantes, Tipos Y UTILES ----
cSalto :: Int
cSalto = 1

cPC :: Registro
cPC = 14

cLR :: Registro
cLR = 13

cSP :: Registro
cSP = 12

cCOMP :: Registro
cCOMP = 15

data Etiqueta = Etiqueta {etiqueta :: String, pc :: Int} -- nombre de la funcion y valor del pc para el salto
  deriving (Eq)

instance Show Etiqueta where
  show (Etiqueta etiqueta pc) = etiqueta ++ " (" ++ show pc ++ ")"

type Programa = [Instruccion]

type Registro = Int

type Memoria = ([Registro], [Int])

type Estado = ([Etiqueta], Memoria) -- (funciones, (registros, pila))

showEstado :: Estado -> String
showEstado (f, (r, p)) = "etiquetas: " ++ show f ++ "\nregistros: " ++ show r ++ "\npila: " ++ show p

getMem :: Estado -> ([Registro], [Int])
getMem (_, m) = m

getReg :: Estado -> [Registro]
getReg (_, (r, _)) = r

getPila :: Estado -> [Int]
getPila (_, (_, p)) = p

getEtiq :: Estado -> [Etiqueta]
getEtiq (f, _) = f

isComentario :: String -> Bool
isComentario linea =
  let i = head (words linea)
   in i == ";" || i == "@" || i == "//" || i == "#"

quitarComas :: String -> String
quitarComas str = [x | x <- str, x /= ',']

compara :: Int -> Int -> Int
compara a b
  | a > b = 1
  | a == b = 0
  | a < b = -1
  | otherwise = 2

-----------------------
---- INSTRUCCIONES ----
-----------------------

procesarInstrucciones :: [String] -> [Etiqueta] -> Programa -> Programa
procesarInstrucciones lineas etiquetas instrucciones =
  -- mientras queden lineas por leer
  if not (null lineas)
    then
      let linea = quitarComas (head lineas) --linea sin comas
          _lineas = tail lineas
          palabras = words linea
          i = head palabras
       in if not (isComentario linea) && last i /= ':' --si es una instruccion y no una etiqueta o un comentario
            then
              let _instrucciones = agregarInstruccion linea etiquetas instrucciones -- agregamos la instruccion a la lista
                in procesarInstrucciones _lineas etiquetas _instrucciones -- procesamos la siguiente linea
            else procesarInstrucciones _lineas etiquetas instrucciones -- ignoramos la linea y procesamos la siguiente     

    else instrucciones -- devolvemos la lista de instrucciones

agregarInstruccion :: String -> [Etiqueta] -> Programa -> Programa
agregarInstruccion linea etiquetas instrucciones =
  let i = procesarInstruccion linea etiquetas
   in if i /= OTRA then instrucciones ++ [i] else instrucciones

procesarInstruccion :: String -> [Etiqueta] -> Instruccion
procesarInstruccion linea etiquetas = do
  let instruccion = words linea
  case head instruccion of
    "ADD" -> ADD (readR (instruccion !! 1)) (readR (instruccion !! 2)) (readR (instruccion !! 3))
    "ADDI" -> ADDI (readR (instruccion !! 1)) (readR (instruccion !! 2)) (readI (instruccion !! 3))
    "SUB" -> SUB (readR (instruccion !! 1)) (readR (instruccion !! 2)) (readR (instruccion !! 3)) -- SUBI porque es SUB con un inmediato
    "SUBI" -> SUBI (readR (instruccion !! 1)) (readR (instruccion !! 2)) (readI (instruccion !! 3))
    "MUL" -> MUL (readR (instruccion !! 1)) (readR (instruccion !! 2)) (readR (instruccion !! 3))
    "MULI" -> MULI (readR (instruccion !! 1)) (readR (instruccion !! 2)) (readI (instruccion !! 3))
    "MOV" -> MOV (readR (instruccion !! 1)) (readR (instruccion !! 2))
    "MOVI" -> MOVI (readR (instruccion !! 1)) (readI (instruccion !! 2))
    "CMP" -> CMP (readR (instruccion !! 1)) (readR (instruccion !! 2))
    "CMPI" -> CMPI (readR (instruccion !! 1)) (readI (instruccion !! 2))
    "BL" -> BLNC (readF (instruccion !! 1) etiquetas)
    "B" -> BNC (readF (instruccion !! 1) etiquetas)
    "BEQ" -> BEQ (readF (instruccion !! 1) etiquetas)
    "BLT" -> BLT (readF (instruccion !! 1) etiquetas)
    "BLE" -> BLE (readF (instruccion !! 1) etiquetas)
    "BGT" -> BGT (readF (instruccion !! 1) etiquetas)
    "BGE" -> BGE (readF (instruccion !! 1) etiquetas)
    "BX" -> BX (readR (instruccion !! 1))
    "PUSH" -> PUSH (readR_2 (instruccion !! 1))
    "POP" -> POP (readR_2 (instruccion !! 1))
    "PrintR" -> PrintR (readR (instruccion !! 1))
    "PrintS" -> PrintS (read (instruccion !! 1))
    _ -> OTRA

-- quitamos la R
readR :: String -> Registro
readR str
  | head str == 'R' = read (drop 1 str)
  | str == "PC" = cPC
  | str == "SP" = cSP
  | str == "LR" = cLR
  | otherwise = - 1

--registro entre llaves
readR_2 :: String -> Registro
readR_2 str = readR (tail (init str)) -- quitamos las llaves


-- quitamos el #
readI :: String -> Int
readI str = read (drop 1 str)


readF :: String -> [Etiqueta] -> Etiqueta
readF str etiquetas
  | str /= "." =  Etiqueta str (buscarPorEtiqueta str etiquetas)
  | otherwise = Etiqueta str (-1)

------------------------------------
---- FUNCIONES DE LOS REGISTROS ----
------------------------------------

-- banco[ri] = valor
setRegistro :: Registro -> Int -> [Registro] -> [Registro]
setRegistro ri valor banco =
  let nuevoBanco = take ri banco ++ [valor] ++ drop (ri + 1) banco
   in nuevoBanco

--------------
---- PILA ----
--------------

push :: Int -> [Int] -> [Int]
push valor pila = valor : pila

pop :: [Int] -> (Int, [Int])
pop pila = (head pila, tail pila) -- (primero, nuevaPila)

-------------------
---- ETIQUETAS ----
-------------------

procesarEtiquetas :: Int -> [String] -> [Etiqueta] -> ([Etiqueta], Int)
procesarEtiquetas pc lineas etiquetas =
  -- mientras queden lineas por leer
  if not (null lineas)
    then
      let linea = head lineas
          _lineas = tail lineas
          palabras = words linea
          i = head palabras
      in if not (isComentario linea) && not (null palabras) && linea /= " "
            then
              if last i == ':' -- etiqueta
                then
                  let _etiquetas = etiquetas ++ [Etiqueta (init i) pc] --guardamos la etiqueta y el pc
                  in procesarEtiquetas pc _lineas _etiquetas -- guardamos el salto y procesamos la siguiente linea
                else procesarEtiquetas ( pc + cSalto ) _lineas etiquetas -- aumentamos el pc y procesamos la siguiente linea sin 
            else procesarEtiquetas pc _lineas etiquetas -- ignoramos la linea y procesamos la siguiente
    else (etiquetas, pc) -- devolvemos la lista de funciones y el pc


-- buscar una funcion en la lista de funciones
buscarPorEtiqueta :: String -> [Etiqueta] -> Int
buscarPorEtiqueta nombre lista =
  let funcion = head [x | x <- lista, etiqueta x == nombre]
   in pc funcion

-----------------------
---- INSTRUCCIONES ----
-----------------------

next :: [Registro] -> [Registro]
next banco = setRegistro cPC ((banco !! cPC) + cSalto) banco

salto :: Int -> [Registro] -> [Registro]
salto pc banco =
    let _nuevoBanco = setRegistro cPC pc banco
        nuevoBanco = setRegistro cLR ((banco !! cPC) + cSalto) _nuevoBanco -- guardamos el pc en el lr
    in nuevoBanco

data Instruccion
  = OTRA
  | ADD Registro Registro Registro
  | ADDI Registro Registro Int
  | SUB Registro Registro Registro
  | SUBI Registro Registro Int
  | MUL Registro Registro Registro
  | MULI Registro Registro Int
  | MOV Registro Registro
  | MOVI Registro Int
  | CMP Registro Registro
  | CMPI Registro Int
  | BNC Etiqueta
  | BLNC Etiqueta
  | BEQ Etiqueta
  | BLT Etiqueta
  | BLE Etiqueta
  | BGT Etiqueta
  | BGE Etiqueta
  | BX Registro
  | PUSH Registro
  | POP Registro
  | PrintR Registro
  | PrintS String
  deriving (Show, Eq)

--  ejecuta una operacion de la siguiente forma:
-- nombre (parametros) banco
ejecutarInstruccion :: Instruccion -> Memoria -> IO Memoria

ejecutarInstruccion (ADD ri ra rb) (banco, pila) = do
  let a = banco !! ra -- banco[ra]
      b = banco !! rb -- banco[rb]
      valor = a + b
      nuevoBanco = setRegistro ri valor banco
  return (next nuevoBanco, pila)

ejecutarInstruccion (ADDI ri ra int) (banco, pila) = do
  let a = banco !! ra -- banco[ra]
      valor = a + int
      nuevoBanco = setRegistro ri valor banco
  return (next nuevoBanco, pila)

ejecutarInstruccion (SUB ri ra rb) (banco, pila) = do
  let a = banco !! ra -- banco[ra]
      b = banco !! rb -- banco[rb]
      valor = a - b
      nuevoBanco = setRegistro ri valor banco
  return (next nuevoBanco, pila)

ejecutarInstruccion (SUBI ri ra int) (banco, pila) = do
  let a = banco !! ra -- banco[ra]
      valor = a - int
      nuevoBanco = setRegistro ri valor banco
  return (next nuevoBanco, pila)

ejecutarInstruccion (MUL ri ra rb) (banco, pila) = do
  let a = banco !! ra -- banco[ra]
      b = banco !! rb -- banco[rb]
      valor = a * b
      nuevoBanco = setRegistro ri valor banco
  return (next nuevoBanco, pila)

ejecutarInstruccion (MULI ri ra int) (banco, pila) = do
  let a = banco !! ra -- banco[ra]
      valor = a * int
      nuevoBanco = setRegistro ri valor banco
  return (next nuevoBanco, pila)

ejecutarInstruccion (MOV ri ra) (banco, pila) = do
  let valor = banco !! ra -- banco[ra]
      nuevoBanco = setRegistro ri valor banco
  return (next nuevoBanco, pila) -- tengo que usar return por lo de IO

ejecutarInstruccion (MOVI ri int) (banco, pila) = do
  let nuevoBanco = setRegistro ri int banco
  return (next nuevoBanco, pila) -- tengo que usar return por lo de IO

ejecutarInstruccion (CMP ri ra) (banco, pila) = do
  let a = banco !! ra -- banco[ra]
      b = banco !! ri -- banco[ri]
      valor = compara a b
      nuevoBanco = setRegistro cCOMP valor banco
  return (next nuevoBanco, pila)

ejecutarInstruccion (CMPI ri int) (banco, pila) = do
  let a = banco !! ri -- banco[ri]
      valor = compara a int
      nuevoBanco = setRegistro cCOMP valor banco
  return (next nuevoBanco, pila)

ejecutarInstruccion (BLNC funcion) (banco, pila) = do
  let r14 = pc funcion -- r14 = func.pc
      nuevoBanco = salto r14 banco
  return (nuevoBanco, pila) -- Salto (no se si tengo que sumarle 4, creo que no)

ejecutarInstruccion (BNC funcion) (banco, pila) = do
  let r14 = pc funcion -- r14 = func.pc
      nuevoBanco = setRegistro cPC r14 banco
  return (nuevoBanco, pila) -- Salto sin guardar el pc

ejecutarInstruccion (BEQ funcion) (banco, pila) = do
  if banco !! cCOMP == 0
    then do
      let r14 = pc funcion -- Asumiendo que 'pc' es un campo o función de 'funcion'
          nuevoBanco = salto r14 banco
      return (nuevoBanco, pila) -- Salto
    else do
      return (next banco, pila)

ejecutarInstruccion (BLT funcion) (banco, pila) = do
  if banco !! cCOMP == -1
    then do
      let r14 = pc funcion -- Asumiendo que 'pc' es un campo o función de 'funcion'
          nuevoBanco = salto r14 banco
      return (nuevoBanco, pila) -- Salto
    else do
      return (next banco, pila)

ejecutarInstruccion (BLE funcion) (banco, pila) = do
  if banco !! cCOMP == 0 || banco !! cCOMP == -1
    then do
      let r14 = pc funcion -- Asumiendo que 'pc' es un campo o función de 'funcion'
          nuevoBanco = salto r14 banco
      return (nuevoBanco, pila) -- Salto
    else do
      return (next banco, pila)

ejecutarInstruccion (BGT funcion) (banco, pila) = do
  if banco !! cCOMP == 1
    then do
      let r14 = pc funcion -- Asumiendo que 'pc' es un campo o función de 'funcion'
          nuevoBanco = salto r14 banco
      return (nuevoBanco, pila) -- Salto
    else do
      return (next banco, pila)

ejecutarInstruccion (BGE funcion) (banco, pila) = do
  if banco !! cCOMP == 0 || banco !! cCOMP == 1
    then do
      let r14 = pc funcion -- Asumiendo que 'pc' es un campo o función de 'funcion'
          nuevoBanco = salto r14 banco
      return (nuevoBanco, pila) -- Salto
    else do
      return (next banco, pila)

ejecutarInstruccion (BX ri) (banco, pila) = do
  let r14 = banco !! ri -- banco[ri]
      nuevoBanco = salto r14 banco -- cambiamos el valor de banco
  return (nuevoBanco, pila) -- Salto

ejecutarInstruccion (PUSH ri) (banco, pila) = do
  let valor = banco !! ri -- banco[ri]
      nuevaPila = push valor pila
  return (next banco, nuevaPila)

ejecutarInstruccion (POP ri) (banco, pila) = do
  let (valor, nuevaPila) = pop pila
      nuevoBanco = setRegistro ri valor banco
  return (next nuevoBanco, nuevaPila)

ejecutarInstruccion (PrintR ri) (banco, pila) = do
  let valor = banco !! ri -- banco[ri]
  print (show ri ++ ": " ++ show valor)
  return (next banco, pila)

ejecutarInstruccion (PrintS str) (banco, pila) = do
  print str
  return (next banco, pila)

ejecutarInstruccion OTRA (banco, pila) = do
  return (next banco, pila)

--------------------------------------------------
--------------------------------------------------
----------------LEER EL CODIGO--------------------
--------------------------------------------------
--------------------------------------------------

main :: IO ()
main = do
  -- banco de registros
  let registros_def =
        [ 0, -- R0
          0, -- R1
          0, -- R2
          0, -- R3
          0, -- R4
          0, -- R5
          0, -- R6
          0, -- R7
          0, -- R8
          0, -- R9
          0, -- R10
          0, -- R11
          0, -- R12/SP
          0, -- R13/LR
          0, -- R14/PC
          2 -- R15/COMPARACION (-1 menor, 0 igual, 1 mayor, otro -> error)
        ] ::
          [Registro]
  -- let instrucciones = [] :: [Instruccion]
  -- let funciones = [] :: [Funcion]
  let pila = [] :: [Int]

  args <- getArgs -- Obtener argumentos de línea de comandos
  if null args
    then putStrLn "Uso: ./interprete nombre_del_archivo [valores de los registros iniciales separados por espacios] [--debug]"
    else do
      let archivo = head args

          debug = length args > 1 && last args == "--debug"
          _args = if debug then init args else args --quitar el --debug

          banco = if length _args > 1
                    then
                      let arg_regs = map read (tail _args) :: [Registro]
                          _registros = arg_regs ++ drop (length arg_regs) registros_def
                        in _registros
                    else registros_def

      let memoria = (banco, pila)

      withFile archivo ReadMode $ \handle -> do
        _codigo <- hGetContents handle
        let codigo = map toUpper _codigo -- lo pasamos todo a mayusculas (ignore case)
        --quitar lineas vacias
        let lineas = [x | x <- lines codigo, x /= "" && not (null (words x))] -- ignoramos las lineas vacias

        let (_etiquetas, _) = procesarEtiquetas 0 lineas []
        let programa = procesarInstrucciones lineas _etiquetas []
        let estado = (_etiquetas, memoria) :: Estado

        when debug $ do
          putStrLn (showEstado estado)
          putStrLn ("programa: " ++ show programa)
          putStrLn "-------------------------EJECTUANDO-------------------------"
          putStrLn "------------------------------------------------------------"

        resultado <- ejecuta programa estado debug
        print resultado


-------------------------
---- EJECUTAR CODIGO ----
-------------------------

ejecuta :: Programa -> Estado -> Bool -> IO Int
ejecuta programa estado debug = do
  -- debug
  when debug $ do 
      putStrLn "\n-------------------------"
      putStrLn (showEstado estado)

  let pc = getReg estado !! cPC -- banco[14]
  --debug
  when debug $ do putStrLn ("pc: " ++ show pc)

  if pc `div` cSalto >= length programa || pc < 0
    then do
      -- debug
      when debug $ do putStrLn "---------------------------FIN------------------------------"

      return (head (getReg estado))-- banco[0]
    else do
      let instruccion = programa !! (pc `div` cSalto) -- Obteniendo la instrucción correspondiente
      
      -- debug
      when debug $ do putStrLn ("instruccion: " ++ show instruccion)

      nuevamem <- ejecutarInstruccion instruccion (getMem estado) --EJECUTAMOS LA INSTRUCCION

      -- debug
      when debug $ do putStrLn ("mem: " ++ show nuevamem)

      -- RECURSION
      ejecuta programa (getEtiq estado, nuevamem) debug

-- no hago una funcion showdebug porque viene bien saber en que parte del codigo estamos, por si hay un fallo o un loop infinito