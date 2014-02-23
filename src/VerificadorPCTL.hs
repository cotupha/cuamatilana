{-|
Módulo que se encarga de verificar un modelo y una
fórmula.
-}
module VerificadorPCTL where

import Modelo
import SintaxisPCTL
import Data.Set (Set)
import qualified Data.Set as Set
import Rela
import Lector
import Data.Array as Array
import Matrix.Matrix
import Matrix.LU
import ArrayUtil

{-|
Se encarga de verificar el modelo y la fórmula especificados
en el archivo cuyo nombre recibe.
Imprime el conjunto de estados que satisfacen a la fórmula.
-}
verifica :: FilePath -> IO()
verifica archivo = do
	((Modelo estados átomos relación etiquetas), fórmulaPCTL) <- (leeArchivo archivo)
	putStrLn (show (edosVálidos estados (obtenEtiquetamientoInverso (Modelo estados átomos relación etiquetas)) relación fórmulaPCTL))

{-|
Obtiene los estados en los que es válida una fórmula dada.
También recibe los estados, la relación de transición con probabilidades y
la función de etiquetamiento inverso.
Regresa el conjunto de estados que satisfacen a la fórmula.
-}
edosVálidos														:: (Set NombreEstado) -> EtiquetamientoInverso -> MatrizProba -> PCTL -> (Set NombreEstado)
edosVálidos estados etiquetamientoInverso relación Top			= estados
edosVálidos estados etiquetamientoInverso relación Bot			= Set.empty
edosVálidos estados etiquetamientoInverso relación (Lit c)		= edosVálidosÁtomo etiquetamientoInverso c 
edosVálidos estados etiquetamientoInverso relación (Not p)		= estados `Set.difference` (edosVálidos estados etiquetamientoInverso relación p)
edosVálidos estados etiquetamientoInverso relación (And p q)	= (edosVálidos estados etiquetamientoInverso relación p) `Set.intersection` (edosVálidos estados etiquetamientoInverso relación q)
edosVálidos estados etiquetamientoInverso relación (Or p q)		= (edosVálidos estados etiquetamientoInverso relación p) `Set.union` (edosVálidos estados etiquetamientoInverso relación q)
edosVálidos estados etiquetamientoInverso relación (Then p q)	= (estados `Set.difference` (edosVálidos estados etiquetamientoInverso relación p)) `Set.union` (edosVálidos estados etiquetamientoInverso relación q)
edosVálidos estados etiquetamientoInverso relación (Prob comp proba (X phi)) = checkX estados etiquetamientoInverso relación comp proba phi
edosVálidos estados etiquetamientoInverso relación (Prob comp proba (Uk num phi psi)) = checkUk estados etiquetamientoInverso relación comp proba num phi psi
edosVálidos estados etiquetamientoInverso relación (Prob comp proba (U phi psi)) = checkU estados etiquetamientoInverso relación comp proba phi psi

{-|
Función encargada de validar si el nombre del átomo
coincide con la cadena que recibe.
-}
encuentraÁtomo		:: Ato -> NombreÁtomo -> Bool
encuentraÁtomo (Átomo c edos) b
	| (c == b)		= True
	| otherwise		= False 

{-|
Dado un etiquetamiento inverso, obtiene los estados que satisfacen
al átomo que recibe como argumento.
-}	
edosVálidosÁtomo :: EtiquetamientoInverso -> NombreÁtomo -> (Set NombreEstado)
--Aquí da igual si utilizamos findMin o findMax, siempre tenemos sólo un elemento en ese conjunto
edosVálidosÁtomo etiquetamientoInverso c = satisfactibleEn(
						Set.findMin(
							Set.filter (\x->encuentraÁtomo x c) etiquetamientoInverso
						)
					  )

{-|
Función encargada de obtener los estados en que es válida una fórmula con el operador
temporal X.
Recibe el conjunto total de estados, la matriz de probabilidades, la comparación, la probabilidad umbral
y la fórmula en PCTL.
-}
checkX :: (Set NombreEstado) -> EtiquetamientoInverso -> MatrizProba -> Rel -> Double -> PCTL -> (Set NombreEstado)
checkX estados etiquetamientoInverso relación comp proba phi = recuperaEstados vectorResultado comp proba
	where
	vectorResultado = mv_mult relación vectorColumna
	vectorColumna = formaVectorColumna (edosVálidos estados etiquetamientoInverso relación phi) numElementos
	((x,y),(z,numElementos)) = bounds relación 

{-|
Función encargada de obtener los estados en que es válida una fórmula con el operador
temporal U acotado.
Recibe el conjunto total de estados, la matriz de probabilidades, la comparación, la probabilidad umbral
y la fórmula en PCTL.
-}
checkUk :: (Set NombreEstado) -> EtiquetamientoInverso -> MatrizProba -> Rel -> Double -> Int -> PCTL -> PCTL -> (Set NombreEstado)
checkUk estados etiquetamientoInverso relación comp proba num phi psi = 
	recuperaEstados estadosUk comp proba
	where
	satPhi = edosVálidos estados etiquetamientoInverso relación phi
	satPsi = edosVálidos estados etiquetamientoInverso relación psi
	sNo = estados `Set.difference` (satPhi `Set.union` satPsi)
	sYes = satPsi
	sP = estados `Set.difference` (sNo `Set.union` sYes)
	prima = construyePrimaUk relación sYes sP
	estadosUk = probabilidadAcotada num sYes prima
	
{-|
Obtiene la matriz de probabilidades auxiliar que se utiliza en la verificación de fórmulas
de U acotado.
-}
construyePrimaUk :: MatrizProba -> (Set NombreEstado) -> (Set NombreEstado) -> MatrizProba
construyePrimaUk relación sYes sP = amap (construyePrimaAuxUk sYes sP) relación
	
{-|
Función auxiliar para formar la matriz utilizada en el U acotado.
Se encarga de ver si el valor se encuentra en sP, en sYes o en ninguno de los dos.
-}
construyePrimaAuxUk :: (Set NombreEstado) -> (Set NombreEstado) -> ((Int,Int), Double) -> ((Int,Int), Double)
construyePrimaAuxUk sYes sP ((a,b),c) 
	| Set.member a sP = ((a,b),c)
	| Set.member a sYes && a == b = ((a,b),1)  
	| otherwise = ((a,b),0)

{-|
Función encargada de calcular la probabilidad de los estados en cada paso,
de manera recursiva.
-}	
probabilidadAcotada :: Int -> (Set NombreEstado) -> MatrizProba -> Array Int Double
probabilidadAcotada 0 sYes prima =  array (1,d) (map (\x -> if (Set.member x sYes) then (x,1.0) else (x,0.0)) [1..d])
	where
	((a,b),(c,d)) = bounds prima
probabilidadAcotada k sYes prima = mv_mult prima (probabilidadAcotada (k-1) sYes prima)

{-|
Función encargada de obtener los estados en que es válida una fórmula con el operador
temporal U no-acotado.
Recibe el conjunto total de estados, la matriz de probabilidades, la comparación, la probabilidad umbral
y la fórmula en PCTL.
-}
checkU :: (Set NombreEstado) -> EtiquetamientoInverso -> MatrizProba -> Rel -> Double -> PCTL -> PCTL -> (Set NombreEstado)
checkU estados etiquetamientoInverso relación comp proba phi psi = 
	recuperaEstados estadosU comp proba
	where
	satPhi = edosVálidos estados etiquetamientoInverso relación phi
	satPsi = edosVálidos estados etiquetamientoInverso relación psi
	sNo = prob0 relación estados satPhi satPsi
	sYes = prob1 relación estados satPhi satPsi sNo
	sP = estados `Set.difference` (sNo `Set.union` sYes)
	vectorColumna = formaVectorColumna sYes (Set.size estados)
	prima = construyePrimaU relación sP
	numElementos = Set.size estados
	sistema = construyeSistema prima numElementos
	vector = formaVectorColumna sYes numElementos
	estadosU = solve sistema vector
	
{-|
Se encarga de construir la matriz prima para resolver que servirá para fomar
el sistema de ecuaciones a resolver.
-}
construyePrimaU :: MatrizProba -> (Set NombreEstado) -> MatrizProba
construyePrimaU relación sP = amap (construyePrimaAuxU sP) relación

{-|
Función auxiliar en la construcción de la matriz prima.
-}	
construyePrimaAuxU :: (Set NombreEstado) -> ((Int, Int), Double) -> ((Int, Int), Double)
construyePrimaAuxU  sP ((a,b),c)
	| Set.member a sP = ((a,b),c)
	| otherwise = ((a,b),0)

{-|
Se encarga de construir la matriz que representará el sistema de ecuaciones
a resolver. Lo hace restando de la matriz identidad de numElementos x numElementos, la matriz prima
calculada.
-}	
construyeSistema :: MatrizProba -> Int -> MatrizProba
construyeSistema prima numElementos = resta (identidad numElementos) prima

{-|
Ejecuta el procedimiento prob0, encargado de encontrar todos los estados desde los que es posible,
con probabilidad 0, alcanzar un estado que satisfaga phiDos, sin dejar los estados que satisfagan phiUno.
-}
prob0 :: MatrizProba -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado)
prob0 relación estados satPhiUno satPhiDos =
	estados `Set.difference` resPrelim
	where
	resPrelim = prob0Aux relación estados satPhiUno satPhiDos

{-|
Función auxiliar para prob0. En realidad es la que hace el trabajo; la inicial sólo le da los valores
con los que debe empezar y ésta simula todo lo que está dentro del while.
-}	
prob0Aux :: MatrizProba -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado)
prob0Aux relación estados satPhiUno r = 
	if not ((r `Set.isSubsetOf` rPrima) && (rPrima `Set.isSubsetOf` r))
					then (prob0Aux relación estados satPhiUno rPrima) 
					else rPrima
	where
	rPrima = r `Set.union` satis
	satis = Set.fold Set.union Set.empty (Set.map(\x->  if Set.size (r `Set.intersection` (imagen relación x)) /= 0 then Set.singleton x else Set.empty) satPhiUno)

{-|
Ejecuta el procedimiento prob1, encargado de encontrar todos los estados en los que la fórmula
se satisface de manera trivial.
-}	
prob1 :: MatrizProba -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado)
prob1 relación estados satPhiUno satPhiDos sNo =
	estados `Set.difference` resPrelim
	where
	resPrelim = prob1Aux relación estados satPhiUno satPhiDos sNo

{-|
Función auxiliar para prob1. En realidad es la que hace el trabajo; la inicial sólo le da los valores
con los que debe empezar y ésta simula todo lo que está dentro del while.
-}	
	
prob1Aux :: MatrizProba -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado) -> (Set NombreEstado)
prob1Aux relación estados satPhiUno satPhiDos r =
	if not ((r `Set.isSubsetOf` rPrima) && (rPrima `Set.isSubsetOf` r)) 
					then (prob0Aux relación estados satPhiUno rPrima) 
					else rPrima 
	where
	rPrima = r `Set.union` satis
	satis = Set.fold Set.union Set.empty (Set.map(\x->  if Set.size (r `Set.intersection` (imagen relación x)) /= 0 then Set.singleton x else Set.empty) (satPhiUno `Set.difference` satPhiDos))

{-|
Función que se encarga de recuperar los estados cuya probabilidad se encuentra en el vector
que recibe como argumento. Valida que la probabilidad de éstos no rebase el umbral dado.
-}
recuperaEstados :: (Array Int Double) -> Rel -> Double -> (Set NombreEstado)
recuperaEstados arreglo Leq proba = Set.fromList 	[ j | j <- comoLista, (arreglo ! j) <= proba]
	where
	b = bounds arreglo 
	comoLista = range b
recuperaEstados arreglo L proba = Set.fromList [ j | j <- comoLista, (arreglo ! j) < proba]
	where
	b = bounds arreglo 
	comoLista = range b 
recuperaEstados arreglo Geq proba = Set.fromList [ j | j <- comoLista, (arreglo ! j) >= proba]
	where
	b = bounds arreglo 
	comoLista = range b
recuperaEstados arreglo G proba = Set.fromList [ j | j <- comoLista, (arreglo ! j) > proba]
	where
	b = bounds arreglo 
	comoLista = range b