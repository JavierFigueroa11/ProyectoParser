{-# OPTIONS_GHC -fno-warn-tabs #-}
module Main where
import qualified ParserProgram as PP
import System.IO
import System.Exit


main :: IO ()
main = do
    putStrLn "----------Menú Parser---------\n Elegir una Opción :\n\n1-Ingresar a los archivos Funcionales.\n2-Ingresar a los archivos no Funcionales\n3-Salir del Programa.\n------------------------------"
    button <- getLine
    case button of
      "1" -> do
          putStrLn "----------Menú Parser---------\nElegir una Opción:\n\n1-Ejecutar Archivo pruebaFuncional1.\n2-Ejecutar Archivo pruebaFuncional2.\n3-Ejecutar Archivo pruebaFuncional3.\n4-Ejecutar Archivo pruebaFuncional4.\n5-Ejecutar Archivo pruebaFuncional5.\n6-Exit the program\n------------------------------"
          button2 <-getLine
          case button2 of
              "1" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaFuncional/pruebaFuncional1.lua"
					  putStrLn $ show parsed
              "2" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaFuncional/pruebaFuncional2.lua"
					  putStrLn $ show parsed
              "3" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaFuncional/pruebaFuncional3.lua"
					  putStrLn $ show parsed
              "4" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaFuncional/pruebaFuncional4.lua"
					  putStrLn $ show parsed
              "5" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaFuncional/pruebaFuncional5.lua"
					  putStrLn $ show parsed
              "6" -> exitSuccess
              _ -> putStrLn "Esta opcion no existe"
      "2" -> do
          putStrLn "----------Menú Parser---------\nChoose an option:\n\n1-Ejecutar Archivo pruebaNoFuncional1.\n2-Ejecutar Archivo pruebaNoFuncional2.\n3-Ejecutar Archivo pruebaNoFuncional3.\n4-Ejecutar Archivo pruebaNoFuncional4.\n5-Ejecutar Archivo pruebaNoFuncional5.\n6-Exit the program\n------------------------------"
          button2 <-getLine
          case button2 of
              "1" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaNoFuncional/pruebaNoFuncional1.lua"
					  putStrLn $ show parsed
              "2" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaNoFuncional/pruebaNoFuncional2.lua"
					  putStrLn $ show parsed
              "3" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaNoFuncional/pruebaNoFuncional3.lua"
					  putStrLn $ show parsed
              "4" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaNoFuncional/pruebaNoFuncional4.lua"
					  putStrLn $ show parsed
              "5" -> do
					  parsed <- PP.parsearArchivo $ "./CarpetaNoFuncional/pruebaNoFuncional5.lua"
					  putStrLn $ show parsed
              "6" -> exitSuccess
              _ -> putStrLn "Esta opcion no existe"
      "3" -> exitSuccess
      _ -> putStrLn "Esta opcion no existe"



