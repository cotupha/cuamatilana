{-|
Módulo en el que se define ĺa sintaxis de las
fórmulas de CTL que se podrán procesar.
-}
module SintaxisPCTL(
	PCTL(..),
	Path(..),
	Rel(..)
	)
	where

{-|
Tipo de dato que representa a una fórmula de estados en PCTL.
-}
data PCTL
	= Top					--
	| Bot
	| Lit String			--Letras proposicionales
	| Not PCTL				--No algo
	| And PCTL PCTL			--algo Y algo
	| Or PCTL PCTL			--algo O algo
	| Then PCTL PCTL		--algo ENTONCES algo
	| Prob Rel Double Path 	-- La probabilidad de que ocurra Path es Rel Double
	deriving(Show)
	
{-|
Tipo de dato que represetna a una fórmula de trayectorias en PCTL.
-}
data Path
	= X PCTL
	| Uk Int PCTL PCTL
	| U PCTL PCTL
	deriving(Show)
	
{-|
Tipo de dato que representa a un comparador en PCTL.
-}
data Rel
	= Leq
	| L
	| Geq
	| G
	deriving(Show)