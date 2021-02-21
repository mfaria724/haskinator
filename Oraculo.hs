module Oraculo (
  Opciones, 
  Oraculo (Prediccion, Pregunta),
  crearOraculo, 
  prediccion, 
  pregunta, 
  opciones, 
  respuesta, 
  ramificar, 
  readOraculo
) where

import Data.Map (Map, fromList, lookup) 
import Data.Maybe (fromJust)

{- DEFINICIONES DE TIPOS -}
type Opciones = Map String Oraculo
data Oraculo = Prediccion String 
            | Pregunta String Opciones 
            deriving(Show, Read)


{- FUNCIONES DE CONSTRUCCION -}
-- Recibe una cadena de caracteres y devuelve un oraculo que consiste 
-- unicamente de la cadena suministrada como prediccion.
crearOraculo :: String -> Oraculo
crearOraculo a = Prediccion a 


{- FUNCIONES DE ACCESO -}
-- Recibe un oraculo y devuelve la cadena de caracteres asociada si el mismo es
-- una prediccion (arroja un error de lo contrario).
prediccion :: Oraculo -> String
prediccion (Prediccion a) = a 

-- Recibe un oraculo y devuelve la cadena de caracteres asociada si el mismo es una 
-- pregunta (arroja un error de lo contrario).
pregunta :: Oraculo -> String
pregunta (Pregunta a _) = a

-- Recibe un oraculo y devuelve la lista de opciones asociadas si el mismo es una 
-- pregunta (arroja un error de lo contrario).
opciones :: Oraculo -> Opciones
opciones (Pregunta _ b) = b

-- Recibe un oraculo y una cadena de caracteres S, y devuelve el oraculo que 
-- corresponde a la respuesta asociada a la opcion S; esto, si el mismo es una 
-- pregunta (arroja un error de lo contrario).
respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta _ b) c = fromJust (Data.Map.lookup c b)  


{- FUNCIONES DE MODIFICACION -}
-- Recibe un lista de cadenas de caracteres (representando opciones a una pregunta),
-- una lista de oraculos y una cadena de caracteres (representando una pregunta). 
-- Devuelve un or ́aculo, de tipo pregunta, con la cadena suministrada y las opciones 
-- construidas.
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar a b c = Pregunta c (fromList (zipWith (\x -> \y -> (x,y)) a b))


{- FUNCIONES DE LECTURA/ESCRITURA -}
-- Lee un string y los convierte al tipo Oraculo.
readOraculo :: String -> Oraculo
readOraculo x = read x :: Oraculo 