{-|
Módulo encargado de leer el archivo en el que se encuentra
la definición del modelo y la fórmula.
Parsea cada una de las partes involucradas.
-}
module Lector (
	leeArchivo
)
where

import SintaxisPCTL
import IO
import Modelo
import Data.Set (Set)
import qualified Data.Set as Set
import Array

{-|
Lee el archivo cuyo nombre recibe como argumento.
Asume que la información en el archivo se encuentra de la siguiente manera:
* Conjunto de átomos
* Número de estados
* Relación de transición
* Función de etiquetamiento
* Fórmula
Regresa la tupla (modelo, fórmula)
-}
leeArchivo :: FilePath -> IO (Modelo, PCTL)                      
leeArchivo filename =

	bracket (openFile filename ReadMode) hClose
	        (\h -> do átomos <- hGetLine h
	                  numEdos <- (hGetLine h) >>= apply read -- Aquí viene el número de estados y lo pasamos a entero.
	                  relación <- leeRelación h 1 numEdos (array ((1,1),(numEdos,numEdos)) [(a,0) | a<-[(i,j) | i<-[1..numEdos], j<-[1..numEdos]]])
	                  etiquetas <- hGetLine h
	                  fórmula <- hGetLine h
	                  return (
	                  	(Modelo
	                  		(Set.fromList[1..numEdos])
	                  		(leeConjunto átomos)
	                  		(relación)
	                  		(leeEtiquetas etiquetas)
	                  	 ),
	                  	(leeFórmula fórmula)
	                   )
	         )
	         
{-|
Función auxiliar para aplicar una función a un "algo".
La función y el "algo" los recibe como argumentos.
-}
apply :: (t -> u) -> t -> IO u
apply f a = return (f a)

{-|
Lee las líneas en que se define la función de transición y
forma la matriz de probabilidad correspondiente.
-}
leeRelación :: Handle -> Int -> Int -> Array (Int, Int) Double -> IO(Array (Int, Int) Double)
leeRelación reader num numEdos matriz=
			loop num matriz
			where
			nextLine = hGetLine reader
			setElementos (e:es) matrizActual = setElementos es (matrizActual // [e])
			setElementos [] matrizActual = matrizActual
			formaRenglón numReng = nextLine >>= apply (leeRelaciónAux numReng)
			formaMatriz i unaMatriz =  do
								elementos <- formaRenglón i
								return (setElementos elementos unaMatriz) 
			loop n matriz = return (n <= (numEdos)) >>= \res -> 
				if res then (
					do
					nuevaMatriz <- formaMatriz n matriz
					(loop (n+1) nuevaMatriz)
					) 
				else return (matriz)
               
{-|
Función auxiliar para leeRelación.
-}
leeRelaciónAux :: Int -> String -> [((Int, Int), Double)]
leeRelaciónAux numReng cadena = fst(head(leeElementosRelación cadena numReng))

{-|
Lee y parsea una cadena de la forma
(a,b),(c,d),(e,f)
Regrega una lista de tuplas ([((Int,Int), Double)], String) donde la cadena siempre
se espera vacía, porque es lo que le falta por analizar.
Todos los elementos de esa lista los agrega a la matriz de probabilidades.
-}

leeElementosRelación :: String -> Int -> [([((Int,Int), Double)], String)] 		
leeElementosRelación s numReng= [
		(((numReng, read b:: Int), read e:: Double):i, j)	| ("(", a) <- lex s,
					(b, c) <- lex a,
					(",",d) <- lex c,
					(e, f) <- lex d,
					(")", g) <- lex f,
					(",",h) <- lex g,
					(i, j) <- leeElementosRelación h numReng
		]
		++
		[
		([((numReng,read b ::Int),read e::Double)], g)	| ("(", a) <- lex s,
					(b, c) <- lex a,
					(",",d) <- lex c,
					(e, f) <- lex d,
					(")", g) <- lex f
		]

{-|
Lee y parsea una cadena de la forma
{a,b,c,d,e}.
Regresa el conjunto (Set) obtenido de la cadena.
-}
leeConjunto :: String -> (Set NombreÁtomo)
leeConjunto conjunto = fst(head(leeConjuntoAux conjunto))

{-|
Lee y parsea una cadena de la forma
{a,b,c,d,e}
Regresa una lista tuplas (conjuntos, String), donde la cadena siempre
se espera vacía, porque es lo que le falta por analizar.
-}
leeConjuntoAux :: String -> [((Set NombreÁtomo), String)]
leeConjuntoAux s = [
		(Set.fromList xs, d) | ("{",a) <- lex s,
							(xs, c) <- leeElementos a,
							("}",d) <- lex c
		]

{-|
Lee y parsea una cadena de la forma
a,b,c,d,e
Regresa una lista tuplas (NombreEstado, String), donde la cadena siempre
se espera vacía, porque es lo que le falta por analizar.
-}
leeElementos :: String -> [([NombreÁtomo], String)]
leeElementos s = [
		(x:c, d)	|	(x, a) <-lex s,
					(",", b) <- lex a,
					(c, d) <- leeElementos b
		]
		++
		[
		([x],a)		| (x,a) <-lex s
		]

{-|
Lee y parsea una cadena de la forma
#s>{a,b,c}#s1>{b,d}
Regresa la función de etiquetamiento que dicha cadena representa.
-}		
leeEtiquetas :: String -> (Set Etiquetamiento)
leeEtiquetas etiquetas = Set.fromList(fst(head(leeEtiquetasAux etiquetas)))
		
{-|
Lee y parsea una cadena de la forma
#s>{a,b,c}#s1>{b,d}
Regresa una lista tuplas (Etiquetamiento, String), donde la cadena es lo
que le falta por analizar.
-}
leeEtiquetasAux :: String -> [([Etiquetamiento], String)]
leeEtiquetasAux s = [
		((Etiqueta (read a::Int) d):i, j)	| ("#", f) <- lex s,
							(a,b) <- lex f,
							(">",c) <- lex b,
							(d, e) <- leeConjuntoAux c,
							(i, j) <- leeEtiquetasAux e
		]
		++
		[
		([(Etiqueta (read a::Int) d)], e)	| ("#", f) <- lex s,
							(a,b) <- lex f,
							(">",c) <- lex b,
							(d, e) <- leeConjuntoAux c
		]

{-|
Lee y parsea fórmulas en PCTL.
Todas las subfórumlas deben estar entre paréntesis y aquéllas que tienen
dos partes, llevan el operador al principio.
Por ejemplo: (And(Lit r)(Lit q))
Regresa la fórmula en CTL
-}	                  
leeFórmula :: String -> PCTL
leeFórmula fórmula = fst(head(leeFórmulaAux fórmula))

{-|
Lee y parsea fórmulas de estados en PCTL.
Regresa una lista tuplas (PCTL, String), donde la cadena es lo
que le falta por analizar.
-}
leeFórmulaAux :: String -> [(PCTL, String)]	
leeFórmulaAux s	=   [
			(Top, x) | 	("(", b) <- lex s,
						("Top", c) <- lex b,
						(")",	x) <- lex c
			]
			++
			[
			(Bot, x) | 	("(", b) <- lex s,
						("Bot", c) <- lex b,
						(")",	x) <- lex c
			]
			++
			[(Lit a, x) | ("(", b) <- lex s,
						 ("Lit", c) <- lex b,
						 (a, u) <- lex c,
						 (")",	x) <- lex u
			]
		   ++
		   [
		   	(Not a, x) | ("(", b) <- lex s,
						 ("Not", c) <- lex b,
						 (a, u) <- leeFórmulaAux c,
						 (")",	x) <- lex u
			]
		   ++
		   [
		   	(And a b, x) | ("(", c) <- lex s,
						   ("And", d) <- lex c,
						   (a, e) <- leeFórmulaAux d,
						   (b, f) <- leeFórmulaAux e,
						   (")",	x) <- lex f
		   ]
		   ++
		   [
		   	(Or a b, x) | ("(", c) <- lex s,
						   ("Or", d) <- lex c,
						   (a, e) <- leeFórmulaAux d,
						   (b, f) <- leeFórmulaAux e,
						   (")",	x) <- lex f
		   ]
		   ++
		   [
		   	(Then a b, x) | ("(", c) <- lex s,
						   ("Then", d) <- lex c,
						   (a, e) <- leeFórmulaAux d,
						   (b, f) <- leeFórmulaAux e,
						   (")",	x) <- lex f
		   ]
		   ++
		   [
		    (Prob comp (read proba ::Double) tray, x) | ("(", c) <- lex s,
		    							("P", d) <- lex c,
		    							(comp, e) <- leeComparador d,
		    							(proba, f) <- lex e,
		    							("[", g) <- lex f,
		    							(tray, h) <- leeTrayectoria g,
		    							("]", i) <- lex h,
		    							(")", x) <- lex i
		   ]

{-|
Lee y parsea fórmulas de trayectorias en PCTL.
Regresa una lista de tuplas (Path, String), donce la cadena es
lo que le falta por analizar.
-}
leeTrayectoria :: String -> [(Path, String)]
leeTrayectoria cadena = [
			(X b, x) | ("X", a) <- lex cadena,
					   (b, x) <- leeFórmulaAux a
			]
			++
			[
			(Uk (read k :: Int) a b, x) | ("Uk", c) <- lex cadena,
						 (k, d) <- lex c,
						 (a, e) <- leeFórmulaAux d,
						 (b, x) <- leeFórmulaAux e
			]
			++
			[
			(U a b, x) | ("U", c) <-lex cadena,
						 (a, d) <- leeFórmulaAux c,
						 (b, x) <- leeFórmulaAux d
			]

{-|
Lee y parsea los operadores de comparación de PCTL.
-}
leeComparador :: String -> [(Rel, String)]
leeComparador cadena = [
			(Leq, a) | ("<=", a) <- lex cadena
			]
			++
			[
			(L, a) | ("<", a) <- lex cadena
			]
			++
			[
			(Geq, a) | (">=", a) <- lex cadena
			]
			++
			[
			(G, a) | (">", a) <- lex cadena
			]