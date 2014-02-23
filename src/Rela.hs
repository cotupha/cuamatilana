{-|
Módulo en el que se define la relación de transición.
-}
module Rela where

import Data.Set (Set)
import qualified Data.Set as Set
import Modelo
import Array

{-|
Obtiene la imagen de un elemento en la relación
|-}
imagen :: MatrizProba -> Int -> Set Int
imagen matriz i =
	Set.fromList (foldl (++) [] (map (\(a,b)-> if matriz ! (a,b) /= 0 then [b] else []) [(a,b) | (a,b) <- indices matriz, a == i]))