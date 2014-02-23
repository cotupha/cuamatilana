{-|
Módulo principal del sistema.
Encargado de obtener el archivo con las definiciones y de procesar
la información que ahí se encuentra (modelo y fórmula)
-}
module Main where

import VerificadorPCTL

{-|
Se encarga de pedir al usuario el nombre del archivo que contiene
la información del modelo y la fórmula.
Procesa la fórmula en cuestión y muestra el resultado en pantalla.
-}
main = do
		putStrLn("Dame el nombre del archivo con la especificación del modelo M y la fórmula a") 
		nombreArchivo <- getLine
		putStrLn("Los estados s tales que M,s|=a son:")
		verifica nombreArchivo