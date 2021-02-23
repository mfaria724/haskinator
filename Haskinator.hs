import Oraculo
import System.IO
import Data.Maybe (fromJust, isNothing)
import Data.Map (fromList, toList) 

{- ###### FUNCIONES PARA VERIFICAR LA CORRECTITUD DE UN ORACULO ####### -}
-- Obtiene una lista con todas las posibles respuestas a una pregunta
obtenerRespuestas :: Opciones -> [String]
obtenerRespuestas x = Prelude.map fst (toList x)

-- Recibe unas opciones y retorna todos los oraculos asociados.
obtenerOraculos :: Opciones -> [Oraculo]
obtenerOraculos x = Prelude.map snd (toList x)

-- Recibe un oraculo y retorna todas las predicciones que aparecen
-- a partir de el.
obtenerPreds :: Oraculo -> [String]
obtenerPreds (Prediccion x) = [x]
obtenerPreds (Pregunta _ x) = desdeOpciones (obtenerOraculos x)
  where
    desdeOpciones :: [Oraculo] -> [String]
    desdeOpciones [] = []
    desdeOpciones (x:xs) = (obtenerPreds x) ++ (desdeOpciones xs)

-- Funcion para obtener todas las predicciones de una lista de oraculos
obtenerPredsOraculos :: [Oraculo] -> [String]
obtenerPredsOraculos [] = []
obtenerPredsOraculos (x:xs) = (obtenerPreds x) ++ (obtenerPredsOraculos xs)

-- Recibe un oraculo y retorna una lista con todas las preguntas que aparecen
-- a partir de el.
obtenerPregs :: Oraculo -> [String]
obtenerPregs (Prediccion x) = []
obtenerPregs (Pregunta text x) = [text] ++ (obtenerPregs' $ obtenerOraculos x)
  where
    obtenerPregs' [] = []
    obtenerPregs' (x:xs) = (obtenerPregs x) ++ (obtenerPregs' xs)



{- ################## FUNCIONES PARA IMPRIMIR CON ESTILO ################## -}
-- Leemos una linea
prompt :: String -> IO String
prompt text = do
    putStr text
    getLine

-- Limpiamos la terminal
clear :: IO ()
clear = do putStr "\ESCc"

-- Regresa el formato de texto a la normalidad
normal :: IO ()
normal = do putStr "\ESC[1;0m"

-- Coloca el texto en negrita
negrita :: IO ()
negrita = do putStr "\ESC[1;1m"

-- Coloca el texto en rojo y negrita
rojo :: IO ()
rojo = do putStr "\ESC[1;31m"

-- Coloca el texto en verde y negrita
verde :: IO ()
verde = do putStr "\ESC[1;32m"

-- Imprime un error
putError :: String -> IO ()
putError text = 
  do clear
     rojo; putStr "*** "; normal; negrita
     putStr text
     rojo; putStr " ***\n\n"; normal

-- Imprime un procedimiento exitoso
putSuccess :: String -> IO ()
putSuccess text = 
  do 
    clear
    verde; putStr "*** "; normal; negrita
    putStr text
    verde; putStr " ***\n\n"; normal



{- ######################### FUNCIONES DE CREACION ######################### -}
crearNuevoOraculo :: IO ()
crearNuevoOraculo = 
  do pred <- prompt "\nIndique la predicción del Oraculo a ser creado:\n"
     putSuccess "Nuevo oraculo creado correctamente!"
     main' (Just $ crearOraculo pred) 



{- ######################### FUNCIONES DE PREDICCION ######################### -}
-- Imprime las opciones disponibles para una pregunta como una lista
-- para que sea legible para el usuario.
printOpciones :: [String] -> IO ()  
printOpciones [] = putStr ""
printOpciones (x:xs) = 
  do
    putStrLn ("* " ++ x)
    printOpciones xs

-- Dada una pregunta, una nueva opcion, una nueva prediccion y un oraculo
-- retorna el mismo oraculo pero modificando la pregunta tal que se agrega
-- la nueva opcion con la nueva prediccion.
agregarOpcion :: String -> String -> String -> Oraculo -> Oraculo
agregarOpcion pregunta opcion prediccion  (Prediccion texto) = (Prediccion texto)
agregarOpcion pregunta opcion prediccion  (Pregunta texto ops)
  -- Si la pregunta actual es la que buscamos, agregamos la nueva opcion y 
  -- la nueva prediccion
  | texto == pregunta = ramificar nuevasOpciones nuevosOraculos pregunta
  -- En caso contrario aplicamos recorsivamente la funcion sobre cada uno
  -- de los sub-oraculos.
  | otherwise = ramificar (obtenerRespuestas ops) opcionesSigNivel texto
  where 
    nuevasOpciones = obtenerRespuestas ops ++ [opcion]
    nuevosOraculos = obtenerOraculos ops ++ [crearOraculo prediccion]
    opcionesSigNivel = map (agregarOpcion pregunta opcion prediccion) (obtenerOraculos ops)

-- Obtenemos una nueva prediccion del usuario.
obtenerNuevaPred :: String -> Oraculo -> [String] -> IO()
obtenerNuevaPred pregunta oraculo opciones = 
  do
    -- Obtenemos la opcion correspondiente.
    opcion <- prompt "Diga la nueva opcion:\n"
    -- Si dicha opcion ya se encuentra, error.
    if elem opcion opciones then do
      putError "Esa opcion ya se encuentra en las anteriores."
      putStrLn "Actuales opciones:"
      printOpciones opciones
      obtenerNuevaPred pregunta oraculo opciones
    else do

      -- Obtenemos la prediccion.
      prediccion <- prompt "Diga la prediccion correspondiente a la nueva opcion:\n"
      -- Si ya existe la prediccion en el oraculo principal, error
      if elem prediccion $ obtenerPreds oraculo then do
        putError "Esa prediccion ya se puede alcanzar usando mis oraculos."
        putStrLn "Actuales predicciones:"
        printOpciones $ obtenerPreds oraculo
        obtenerNuevaPred pregunta oraculo opciones
      else do

        -- Actualizamos el oraculo.
        putSuccess "Oraculo actualizado correctamente!"
        let oraculo' = agregarOpcion pregunta opcion prediccion oraculo
        main' $ Just oraculo'
    
-- Ejecuta el proceso de predicción hasta que llega a una de las hojas del
-- arbol o hasta que el usuario contesta ninguna de las anteriores.
mainPrediccion :: Oraculo -> Oraculo -> IO ()
-- Si llegamos a una hoja, damos la prediccion
mainPrediccion (Prediccion texto) mainOraculo = 
  do 
    putStrLn ("Usted está pensando en: " ++ texto ++ "\n")
    respuesta <- prompt "¿Es esto correcto? (Si o No)\n"
    putStr "\n"
    if respuesta == "Si"
      then do
        -- Si predecimos correctamente, regresamos al main.
        putSuccess "Los humanos son demasiado predecibles..."
        main' $ Just mainOraculo
      else do
        putStrLn "Como que no mmgvo? Estas en drogas?"
-- Si llegamos a una pregunta, pedimos una opcion al usuario.
mainPrediccion (Pregunta texto opciones) mainOraculo = 
  do
    negrita
    putStrLn (texto)
    normal
    let opcionesStr = obtenerRespuestas opciones 
    printOpciones opcionesStr 
    putStrLn "* Ninguna de las anteriores"
    input <- prompt "Ingrese una de las opciones:\n"
    putStr "\n"
    let oraculo = (Pregunta texto opciones)
    -- Verificamos que es una opcion valida y pasamos al siguiente oraculo.
    if elem input opcionesStr then do
      mainPrediccion (respuesta oraculo input) mainOraculo
    -- Si no es ninguna de las opciones anteriores, tenemos que agregar
    -- una nueva prediccion.
    else if input == "Ninguna de las anteriores" then do
      obtenerNuevaPred texto mainOraculo $ obtenerRespuestas opciones
    -- La opcion no es valida, error
    else do
      putError "Por favor, seleccione una opcion correcta."
      mainPrediccion oraculo mainOraculo

-- Inicia el proceso de prediccion.
iniciarPrediccion :: Oraculo -> Oraculo -> IO ()
iniciarPrediccion oraculo mainOraculo =
  do
    clear
    negrita
    putStrLn "#################################"
    putStrLn "¡Inicia el proceso de prediccion!"
    putStrLn "#################################\n"
    normal
    mainPrediccion oraculo mainOraculo



{- ######################## FUNCIONES DE PERSISTENCIA ######################## -}
persistirOraculo :: Maybe Oraculo -> IO ()
persistirOraculo oraculo
  | isNothing oraculo = 
      do putError "No se ha cargado en memoria ningún oraculo."
         main' Nothing
  | otherwise =
      do fileName <- prompt "\nIndique el nombre del archivo de salida:\n" 
         file <- openFile fileName WriteMode
         hPutStrLn file (show $ fromJust oraculo)
         hClose file
         putSuccess "El Oraculo ha sido guardo con éxito"
         main' oraculo



{- ########################## FUNCIONES DE CARGADO ########################## -}
cargarOraculo :: IO ()
cargarOraculo =
  do fileName <- prompt "\nIndique el nombre del archivo de entrada:\n" 
     file <- openFile fileName ReadMode
     putStrLn ""
     textoOraculo <- hGetLine file
     let oraculo = readOraculo textoOraculo
     hClose file
     putSuccess "El Oraculo ha sido cargado con éxito"
     main' (Just oraculo)



{- ############### FUNCIONES DE CONSULTA DE PREGUNTA CRUCIAL ############### -}
-- Ejecuta un recorrido DFS en el oraculo y devuelve una lista con el camino
-- hasta el nodo buscado.
dfs :: Oraculo -> String -> Maybe [String]
dfs (Prediccion texto) a
  | a == texto = Just [a]
  | otherwise = Nothing
dfs (Pregunta texto opciones) a = dfs' (toList opciones) a [texto]

-- Funcion auxiliar de dfs, para desarmar el nodo cuando es una pregunta
-- y poder bifurcar hacia cada una de las opciones
dfs' :: [(String, Oraculo)] -> String -> [String] -> Maybe [String]
dfs' [] a r = Nothing
dfs' ((x1,x2):xs) a r 
  | r' == Nothing = dfs' xs a r
  | otherwise = Just (r ++ [x1] ++ (fromJust r'))
  where 
    r' = dfs x2 a

-- Copara el resultado de dos caminos creados por el dfs y retorna la pregunta 
-- donde ambos se bifurcan.
obtenerPuntoInflexion :: [String] -> [String] -> String -> Maybe String
obtenerPuntoInflexion [] _ _ = Nothing
obtenerPuntoInflexion _ [] _ = Nothing
obtenerPuntoInflexion (x:xs) (y:ys) r
  | x == y = obtenerPuntoInflexion xs ys x
  | otherwise = Just r

consultarPreguntaCrucial :: Maybe Oraculo -> IO ()
-- Chequear cuando el oraculo es Nothing
consultarPreguntaCrucial oraculo =
  do pred1 <- prompt "Por favor, ingrese la primera predicción:\n"
     pred2 <- prompt "Por favor, ingrese la segunda predicción:\n"
     let predsOrcaulo = obtenerPreds (fromJust oraculo)
     if (and [elem pred1 predsOrcaulo, elem pred2 predsOrcaulo])
       then do 
         let l1 = dfs (fromJust oraculo) pred1
         let l2 = dfs (fromJust oraculo) pred2
         let ancestro = obtenerPuntoInflexion (fromJust l1) (fromJust l2) ""
         putSuccess ("La pregunta crucial es: " ++ (fromJust ancestro))
       else do 
         putError "Su consulta es inválida"
     main' oraculo



{- ########################## FUNCIONES DEL MAIN ########################## -}
iniciaFuncion :: Maybe Oraculo -> Char -> IO ()
iniciaFuncion oraculo op
  | op == '1' = do crearNuevoOraculo
  | op == '2' = do 
      if isNothing oraculo then do
        putError "No se puede iniciar una prediccion sin ningun oraculo cargado."
        main' oraculo
      else do 
        iniciarPrediccion (fromJust oraculo) (fromJust oraculo)
  | op == '3' = do 
      if isNothing oraculo then do
        putError "No se puede guardar un oraculo si no hay ninguno cargado."
        main' oraculo
      else do 
        persistirOraculo oraculo
  | op == '4' = do cargarOraculo
  | op == '5' = do 
      if isNothing oraculo then do
        putError "No se puede consultar una pregunta crucial si no hay ningun oraculo cargado."
        main' oraculo
      else do 
        consultarPreguntaCrucial oraculo
  | op == '6' = do putStrLn (show oraculo)
                   main' oraculo
  | otherwise = do putStrLn "\nHasta la proxima!" 

-- Verifica si un caracter esta en un string.
verificarOpcion :: Char -> String -> Bool
verificarOpcion _ "" = False
verificarOpcion op (x:xs)
  | op == x = True
  | otherwise = verificarOpcion op xs  

main' :: Maybe Oraculo -> IO ()
main' oraculo = 
  do putStrLn "Por favor, selecciona una de las siguientes opciones:\n"
     putStrLn "1. Crear un Oraculo nuevo"
     putStrLn "2. Predecir"
     putStrLn "3. Persistir"
     putStrLn "4. Cargar"
     putStrLn "5. Consultar pregunta crucial"
     putStrLn "6. Salir"
     opcion <- getChar
     putStr "\n"
     if (not (verificarOpcion opcion ['1'..'7']))
       then do putError "Opción inválida."
               main' oraculo
       else do iniciaFuncion oraculo opcion      

main :: IO ()
main = 
  do 
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    clear
    negrita
    putStrLn "#########################"
    putStrLn "¡Bienvenido a Haskinator!"
    putStrLn "#########################\n"
    normal
    main' Nothing  


{- BORRAR -}
oraculoTest = crearOraculo "Eres de Guarico"
opcionesTest = fromList [
  ("Si", oraculoTest), 
  ("No", crearOraculo "Eres de Caracas")
  ]

oraculosNivel3 = [
  crearOraculo "Jared Leto",
  crearOraculo "Heath Ledger"
  ]

oraculosNivel2 = [
  ramificar ["Suicide Squad", "The Dark Knight"] oraculosNivel3 "De que pelicula es el Joker?",
  crearOraculo "Tom Hardy",
  crearOraculo "Cillian Murphy"
  ]

oraculosNivel1 = [
  ramificar ["Joker", "Bane", "Scarecrow"] oraculosNivel2 "Cual es el nombre del villano?",
  crearOraculo "Christian Bale"
  ]

oraculoEnunciado = ramificar ["Si", "No"] oraculosNivel1 "El actor interpreto un villano?"