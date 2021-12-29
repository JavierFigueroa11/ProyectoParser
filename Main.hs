{-# OPTIONS_GHC -fno-warn-tabs #-}

module Main where
import qualified ParserProgram as PP
import System.IO
import System.Exit

main :: IO ()
main = do
    putStrLn "----------Menú Parser---------\n Elegir una Opción :\n\n1-Ingresar a los archivos Funcionales.\n2-Salir del Programa.\n------------------------------"
    button <- getLine
    case button of
      "1" -> do
          putStrLn "----------Menú Parser---------\nElegir una Opción:\n\n1-Ejecutar Archivo pruebaFuncional1.\n2-Ejecutar Archivo pruebaFuncional2.\n3-Ejecutar Archivo pruebaFuncional3.\n4-Ejecutar Archivo pruebaFuncional4.\n5-Ejecutar Archivo pruebaFuncional5.\n6-Exit the program\n------------------------------"
          bloopLine
      "2" -> exitSuccess
      _ -> putStrLn "Esta opcion no existe"
 
bloop f = PP.parsearArchivo f >>= print
bloop' n = bloop ("./CarpetaFuncional/pruebaFuncional" <> n <> ".lua")
bloop'' b =
  if b `elem` (pure <$> ['1' .. '5'])
  then
    bloop' b
  else if b == "6"
    then
      exitSuccess
    else
      putStrLn "Esta opcion no existe"
bloopLine =
  getLine >>= bloop''
