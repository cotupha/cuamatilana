{-|
Módulo que contiene funciones auxiliares para el manejo de arreglos y matrices.
-}
module ArrayUtil

where

import Modelo
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Array as Array

{-|
Recibe un conjunto de estados y forma un vector en el que pone un 1 en la
posición i si el estado i está en el conjunto recibido.
-}
formaVectorColumna :: (Set NombreEstado) -> Int -> (Array Int Double)
formaVectorColumna estados numEstados = array ((1,numEstados)) parejasAux
	where parejasAux = (map (formaVectorColumnaAux estados) [1..numEstados])
	
{-|
Función auxiliar para formaVectorColumna.
-}
formaVectorColumnaAux :: (Set NombreEstado) -> Int -> (Int, Double)
formaVectorColumnaAux estados buscado 
	| Set.member buscado estados = (buscado, 1.0)
	| otherwise = (buscado, 0.0)

{-|
Se encarga de aplicar a todas las entradas de la matriz (inlcuyendo a sus
índices, una función.
-}
amap :: (((Int, Int), Double)->((Int, Int), Double)) -> MatrizProba -> MatrizProba
amap f matriz = array (bounds matriz) (map (\((a,b),c)-> f((a,b),c)) (assocs matriz))

{-|
Crea la matriz identidad de nxn.
-}
identidad :: Int -> Array (Int,Int) Double 
identidad n = array b (map (\(w,x) -> (if w==x then ((w,x),1) else ((w,x),0))) (range b))
	where b = ((1,1),(n,n))

{-|
Obtiene el resultado de restar a - b
-}
resta :: MatrizProba -> MatrizProba -> MatrizProba	
resta a b = array (bounds a) (restaAux (assocs a) (assocs b))

{-|
Función auxiliar para resta.
-}
restaAux :: [((Int, Int),Double)] -> [((Int, Int),Double)] -> [((Int, Int),Double)] 
restaAux a b = zipWith (\((i,j),k) ((l,m),n)-> ((i,j),k-n)) a b