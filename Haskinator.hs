import Oraculo
import System.IO
import Data.Maybe
import Data.List

dfs oraculo [] = 
dfs oraculo (x:xs) = 

prompt :: String -> IO String
prompt text = do
    putStr text
    getLine

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

cargarOraculo :: IO ()
cargarOraculo =
  do fileName <- prompt "\nIndique el nombre del archivo de entrada:\n" 
     file <- openFile fileName ReadMode
     putStrLn ""
     textoOraculo <- hGetLine file
     let oraculo = readOraculo textoOraculo
     putStrLn "El Oraculo ha sido cargado con éxito!"
     main' (Just oraculo)

persistirOraculo :: Maybe Oraculo -> IO ()
persistirOraculo oraculo
  | isNothing oraculo = 
      do putStrLn "\nNo se ha cargado en memoria ningún oraculo.\n"
         main' Nothing
  | otherwise =
      do fileName <- prompt "\nIndique el nombre del archivo de salida:\n" 
         file <- openFile fileName WriteMode
         putStrLn ""
         hPutStrLn file (show $ fromJust oraculo)
         putStrLn "El Oraculo ha sido guardo con éxito!"
         main' oraculo

crearNuevoOraculo :: IO ()
crearNuevoOraculo = do pred <- prompt "\nIndique la predicción del Oraculo a ser creado:\n"
                       main' (Just $ crearOraculo pred) 

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
  | otherwise = do putStrLn "Hasta la proxima!" 

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
     if (not (verificarOpcion opcion ['1'..'7']))
       then do putStrLn "\ESCcPor favor, selecciona una opción válida.\n"
               main' oraculo
       else do iniciaFuncion oraculo opcion      

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          hSetBuffering stdin NoBuffering
          putStrLn "#########################"
          putStrLn "¡Bienvenido a Haskinator!"
          putStrLn "#########################\n"
          main' Nothing

verificarOpcion :: Char -> String -> Bool
verificarOpcion _ "" = False
verificarOpcion op (x:xs)
  | op == x = True
  | otherwise = verificarOpcion op xs    