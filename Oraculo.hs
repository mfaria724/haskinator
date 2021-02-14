module Oraculo where

import Data.Map 
import Data.String
import Data.Maybe
import Text.Read

-- Definiciones de Tipos
type Opciones = Map String Oraculo
data Oraculo = Prediccion String 
             | Pregunta String Opciones 
             deriving(Show, Read)

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

-- Read con casting
readOraculo :: String -> Oraculo
readOraculo x = read x :: Oraculo

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
oraculoConOpciones = Pregunta "Te gusta el queso?" opcionesTest

opcionesRamificar = ["Suicide Squad", "The Dark Knight"]
oraculosRamificar = [
  crearOraculo "Jared Leto",
  crearOraculo "Heath Ledger"
  ]

a = ramificar opcionesRamificar oraculosRamificar "Que palicula?"
p = show oraculoConOpciones