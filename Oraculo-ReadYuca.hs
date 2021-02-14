module Oraculo where

import Data.Map 
import Data.String
import Data.Maybe
import Text.Read

-- Como nos encanta Haskell, decidimos que queríamos hacer nuestra propia
-- implementación de Read y Show (la verdad es que no lo entendemos, pero
-- no podiamos decir eso en el comentario).

-- Definiciones de Tipos
type Opciones = Map String Oraculo
data Oraculo = Prediccion String | Pregunta String Opciones

-- Funciones de construcción
crearOraculo :: String -> Oraculo
crearOraculo a = Prediccion a 

-- Funciones de acceso
prediccion :: Oraculo -> String
prediccion (Prediccion a) = a 

pregunta :: Oraculo -> String
pregunta (Pregunta a _) = a

opciones :: Oraculo -> Opciones
opciones (Pregunta _ b) = b

respuesta :: Oraculo -> String -> Oraculo
respuesta (Pregunta a b) c = fromJust (Data.Map.lookup c b)  

-- Funciones de modificación
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar a b c = Pregunta c (fromList (zipWith (\x -> \y -> (x,y)) a b))

-- Instancias
instance Show Oraculo where
  show (Prediccion a) = "#" ++ a
  show (Pregunta a b) = "@" ++ a ++ "{" ++ (showOpciones (toList b)) 
                        ++ (showPregunta (toList b)) ++ "}"
    where 
      showPregunta [] = ""
      showPregunta ((x,y):xs) = show y ++ showPregunta xs
      showOpciones [] = ""
      showOpciones ((x,y):xs) = "|" ++ x ++ showOpciones xs  


oraculoTest = crearOraculo "Eres de Guarico"
opcionesTest = fromList [
  ("Si", oraculoTest), 
  ("No", crearOraculo "Eres de Caracas")
  ]

-- Aux Func
readS ('}':s) r = r
readS ('@':s) r = r
readS ('#':s) r = r

readS (c:s) r = readS s (r ++ [c])
readS "" r = r

readPregunta ('{':s) r = r 
readPregunta (c:s) r = readPregunta s (r ++ [c])

readOpciones ('{':s) = readOpciones' s []
readOpciones (c:s) = readOpciones s

readOpciones' ('|':s) r = readOpciones' s (r ++ [""])
readOpciones' ('@':s) r = r
readOpciones' ('#':s) r = r
readOpciones' (c:s) r = readOpciones' s (addChar r c [])
  where 
    addChar [r] c a = a ++ [r ++ [c]]
    addChar (r:rs) c a = addChar rs c (a ++ [r])

readsOraculo ('#':s) = Prediccion (readS s "")
readsOraculo ('@':s) = ramificar (readOpciones s) (readOraculos s) (readPregunta s "")

nextOraculo ('#':s) = '#':s
nextOraculo ('@':s) = '@':s
nextOraculo ('}':s) = "}"
nextOraculo ('{':s) = finalizarPregunta s 1 
  where 
    finalizarPregunta s 0 = s
    finalizarPregunta ('{':s) n = finalizarPregunta s (n+1)
    finalizarPregunta ('}':s) n = finalizarPregunta s (n-1)
    finalizarPregunta (c:s) n = finalizarPregunta s n
nextOraculo (c:s) = nextOraculo s

readOraculos' ('}':s) r = r
readOraculos' "" r = r
readOraculos' ('@':s) r = readOraculos' (nextOraculo s) (r ++ [readsOraculo ('@':s)])
readOraculos' ('#':s) r = readOraculos' (nextOraculo s) (r ++ [readsOraculo ('#':s)])
    

readOraculos ('#':s) = readOraculos' ('#':s) []
readOraculos ('@':s) = readOraculos' ('@':s) []
readOraculos (c:s) = readOraculos s

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
  crearOraculo "Christian Bale"oraculo
oraculoConOpciones = Pregunta "Te gusta el queso?" opcionesTest

opcionesRamificar = ["Suicide Squad", "The Dark Knight"]
oraculosRamificar = [
  crearOraculo "Jared Leto",
  crearOraculo "Heath Ledger"
  ]

a = ramificar opcionesRamificar oraculosRamificar "Que palicula?"
p = show oraculoConOpciones