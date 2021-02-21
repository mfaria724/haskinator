import Oraculo
import System.IO
import Data.Maybe (fromJust, isNothing)
import Data.Map (fromList, toList) 

{- FUNCIONES PARA VERIFICAR LA CORRECTITUD DE UN ORACULO -}
-- Retorna el segundo elemento de una tupla de dos elementos.
second :: (a, b) -> b 
second (_, y) = y

-- Recibe unas opciones y retorna todos los oraculos asociados.
obtenerOraculos :: Opciones -> [Oraculo]
obtenerOraculos x = Prelude.map second (toList x)

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

-- Funcion para verificar si todos los elementos de una lista son distintos.
distintos :: Eq a => [a] -> Bool
distintos [] = True
distintos (x:xs) 
  | elem x xs = False
  | otherwise = distintos xs

-- Funcion para verificar si una lista de oraculos es correcta.
verificarPreds :: [Oraculo] -> Bool
verificarPreds x = distintos $ obtenerPredsOraculos x 


{- FUNCIONES PARA IMPRIMIR CON ESTILO -}
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
  do clear
     verde; putStr "*** "; normal; negrita
     putStr text
     verde; putStr " ***\n\n"; normal


{- FUNCIONES DE CREACION -}
crearNuevoOraculo :: IO ()
crearNuevoOraculo = 
  do pred <- prompt "\nIndique la predicción del Oraculo a ser creado:\n"
     putSuccess "Nuevo oraculo creado correctamente!"
     main' (Just $ crearOraculo pred) 


{- FUNCIONES DE PREDICCION -}


{- FUNCIONES DE PERSISTENCIA -}
persistirOraculo :: Maybe Oraculo -> IO ()
persistirOraculo oraculo
  | isNothing oraculo = 
      do putError "No se ha cargado en memoria ningún oraculo."
         main' Nothing
  | otherwise =
      do fileName <- prompt "\nIndique el nombre del archivo de salida:\n" 
         file <- openFile fileName WriteMode
         hPutStrLn file (show $ fromJust oraculo)
         putSuccess "El Oraculo ha sido guardo con éxito"
         main' oraculo


{- FUNCIONES DE CARGADO -}
cargarOraculo :: IO ()
cargarOraculo =
  do fileName <- prompt "\nIndique el nombre del archivo de entrada:\n" 
     file <- openFile fileName ReadMode
     putStrLn ""
     textoOraculo <- hGetLine file
     let oraculo = readOraculo textoOraculo
     putSuccess "El Oraculo ha sido cargado con éxito"
     main' (Just oraculo)


{- FUNCIONES DE CONSULTA DE PREGUNTA CRUCIAL -}
-- dfs oraculo [] = 
-- dfs oraculo (x:xs) = 

consultarPreguntaCrucial :: Maybe Oraculo -> IO ()
-- Chequear cuando el oraculo es Nothing
consultarPreguntaCrucial oraculo =
  do pred1 <- prompt "Por favor, ingrese la primera predicción:\n"
     pred2 <- prompt "Por favor, ingrese la segunda predicción:\n"
     let predsOrcaulo = obtenerPreds (fromJust oraculo)
     if (and [elem pred1 predsOrcaulo, elem pred2 predsOrcaulo])
       then do putStrLn "Obtener ancestro"
       else do putStrLn "Su consulta es inválida"
     main' oraculo


{- FUNCIONES DEL MAIN -}
iniciaFuncion :: Maybe Oraculo -> Char -> IO ()
iniciaFuncion oraculo op
  | op == '1' = do crearNuevoOraculo
  | op == '2' = do putStrLn "Opcion 2"
                   putStrLn (show oraculoEnunciado)
  | op == '3' = do persistirOraculo oraculo
  | op == '4' = do cargarOraculo
  | op == '5' = do consultarPreguntaCrucial oraculo
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
  do hSetBuffering stdout NoBuffering
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
  crearOraculo "Jared Leto",
  crearOraculo "Cillian Murphy"
  ]

oraculosNivel1 = [
  ramificar ["Joker", "Bane", "Scarecrow"] oraculosNivel2 "Cual es el nombre del villano?",
  crearOraculo "Christian Bale"
  ]

oraculoEnunciado = ramificar ["Si", "No"] oraculosNivel1 "El actor interpreto un villano?"