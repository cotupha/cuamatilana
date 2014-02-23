{-|
Módulo que contiene las definiciones de tipos de datos
necesarias para especificar un modelo.
-}
module Modelo(
	NombreEstado,
	NombreÁtomo,
	MatrizProba,
	Ato(..),
	Etiquetamiento(..),
	Modelo(..),
	EtiquetamientoInverso,
	obtenEtiquetamientoInverso,
	satisfactibleEn
	)
	where
	
import Data.Set (Set)
import qualified Data.Set as Set
import Array

{-|
El nombre de un estado es un entero
-}
type NombreEstado = Int

{-|
El nombre de un átomo es una cadena
-}
type NombreÁtomo = String

type MatrizProba = Array (Int, Int) Double

{-|
Tipo de dato para representar las etiquetas
de un estado
-}
data Etiquetamiento =
	Etiqueta NombreEstado (Set NombreÁtomo)
	deriving (Eq, Ord, Show)

{-|
Tipo de dato para representar un átomo junto
con los estados que lo satisfacen
-}
data Ato =
	Átomo NombreÁtomo (Set NombreEstado)
	deriving (Eq, Ord, Show)

{-|
Tipo de dato para representar un modelo.
Contiene el conjunto de estados, el conjunto de átomos,
la relación de transición y la función de etiquetamiento.
-}	
data Modelo =
	Modelo (Set NombreEstado) (Set NombreÁtomo) MatrizProba (Set Etiquetamiento) 

{-|
El etiquetamiento inverso es un conjunto de átomos con los estados
que lo satisfacen.
-}	
type EtiquetamientoInverso = Set Ato

{-|
Obtiene el etiquetamiento inverso de un modelo dado
(función L' que se usa en la verificación)
-}
obtenEtiquetamientoInverso :: Modelo -> EtiquetamientoInverso
obtenEtiquetamientoInverso (Modelo estados átomos relación etiquetas) = 
	obtenEtiquetamientoInversoAux etiquetas átomos

{-|
Función auxiliar para el etiquetamiento inverso.
-}
obtenEtiquetamientoInversoAux :: (Set Etiquetamiento) -> (Set NombreÁtomo) -> (Set Ato)
obtenEtiquetamientoInversoAux etiquetas átomos =
	(Set.map (\x -> obtenEstadosQueSatisfacen etiquetas x) átomos)

{-|
Forma un átomo con su nombre y los estados que lo
satisfacen
-}		
formaÁtomo :: NombreÁtomo -> (Set NombreEstado) -> Ato
formaÁtomo nombreÁtomo estados = Átomo nombreÁtomo estados

{-|
Recibe un conjunto de etiquetas y un átomo.
Veo en cuáles etiquetas está el átomo y agrego el estado
correspondiente.
-}
obtenEstadosQueSatisfacen :: (Set Etiquetamiento) -> NombreÁtomo -> Ato
obtenEstadosQueSatisfacen cjtoEtiquetas átomo =
	formaÁtomo átomo (Set.fold Set.union Set.empty (Set.map (\x -> obtenEstadosQueSatisfacenAux x átomo) cjtoEtiquetas))

{-|
Recibe una etiqueta y un átomo.
Ve si el átomo está en la etiqueta y, de ser así,
regresa el estado correspondiente. Si no, regresa el vacío.
-}
obtenEstadosQueSatisfacenAux :: Etiquetamiento -> NombreÁtomo -> (Set NombreEstado)
obtenEstadosQueSatisfacenAux (Etiqueta nombreEstado cjtoÁtomos) átomo
	| Set.member átomo cjtoÁtomos = (Set.singleton nombreEstado)
	| otherwise = Set.empty

{-|
Recibe un átomo completo (no sólo el nombre).
Regresa los estados
en que se satisface ese átomo.
-}
satisfactibleEn	:: Ato -> Set NombreEstado
satisfactibleEn (Átomo c edos) = edos