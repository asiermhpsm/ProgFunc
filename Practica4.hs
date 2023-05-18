import Data.Char (isAlpha, toLower)
import Data.List 

{-
Funcionamiento general del codigo:
Primero se generarán toda las combinaciones posibles con las letras disponibles. A continuación se eliminaran las que no cumplan unas características generales (como que no es posible que haya 4 consonantes seguidas) 
y luego se comprobará que cumplan otras reglas relacionadas a las sílabas (como que consonantes al final de sílaba solo puede ser l, n, s,r). 
Estas reglas de las silabas se harán en profundiad, es decir que dada la palabra se intentara separar en sílabas 
de todas la maneras posibles de manera que cumplan con las reglas llegando al final de una separación antes de probar con otra y si encuentra una separación que satisface las reglas no se busca más.
Reglas generales:
-No puede haber 4 consonantes seguidas
...........
Reglas de sílabas:
-consonantes al final de sílaba solo puede ser l, n, s,r
..........
-}

{-
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
MAIN
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-}
main :: IO ()
main = do
  putStrLn "Este programa facilita el juego del Scrable. Se pedirá al usuario que ingrese las letras que tiene disponibles, así como las letras disponibles en el tablero y se le devolverá las palabras que puede formar. "
  putStrLn "Tenga en cuenta que de lo que escriba el usuario solo se tendrán en cuenta las letras."
  loop

loop :: IO ()
loop = do
  putStrLn "\nIngrese sus letras (o escriba 'salir' para finalizar):"
  input1 <- getLine
  if input1 /= "salir"
    then do
      putStrLn "Ingrese las letras del tablero:"
      input2 <- getLine
      let resultado = procesar (soloLetrasMinusculas (input1 ++ input2))
      putStrLn "Aquí tienes algunas palabras que puedes usar:"
      putStrLn $ intercalate ", " resultado
      loop
    else putStrLn "\nSaliendo...."



--Dada una lista de letras te da todas las posibles palabras validas ordenadas de mayor a menor longitud
procesar :: String ->  [String]
procesar str = ordenarPorLongitud (filter palabraValida( combinaciones str ))


{-
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
FUNCIONES AUXILIARES
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-}

--Devuelve true si es consonante
esVocal :: Char -> Bool
esVocal c = c `elem` "aeiou"

--Devuelve true si es consonante
esConsonante :: Char -> Bool
esConsonante c = not (esVocal c) && c `elem` ['a'..'z'] 

--Dado un string devuelve solo las letras y convertidas todas a minuscula
soloLetrasMinusculas :: String -> String
soloLetrasMinusculas str = map toLower (filter isAlpha str)

--Ordena una lista de string por longitud de palabra
ordenarPorLongitud :: [String] -> [String]
ordenarPorLongitud = sortBy (\x y -> compare (length y) (length x))

{-
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
COMBINACIONES
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-}
--Dadas unas letras devuelve todas las posibles combinaciones con al menos dos letras sin elementos repetidos. (copiada de internet)
combinaciones :: String -> [String]
combinaciones str = nub $ filter (\s -> length s >= 2) $ concatMap subsequences (tails str)

{-
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
VALIDAR PALABRA
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
A partir de ahora se supone correcto que el parametro de entrada es una lista de letras en minuscula, sin números ni ningún tipo de simbolo
-}
--Devuelve true si la palabra es valida con las reglas definidas
palabraValida :: String -> Bool
palabraValida str = palabraValidaGeneral str && palabraValidaPorSilabas str

--Realiza comprobaciones generales sobre la palabra y devuelve si es valida 
palabraValidaGeneral :: String -> Bool
palabraValidaGeneral palabra =
  let
    funciones = [noMasTresConsonantesSeguidas]  -- Lista de funciones de reglas generales a comprobar TODO
  in
    all (\f -> f palabra) funciones

--Hace un busqueda en profundidad para ver si una palabra es valida separandola por silabas y comprobando cada silaba, supone silabas de hasta 4 letras
palabraValidaPorSilabas :: String -> Bool
palabraValidaPorSilabas [] = True
palabraValidaPorSilabas (x:y:z:w:resto) = (silabaValida [x, y, z, w] && palabraValidaPorSilabas resto)
                                                            || (silabaValida [x, y, z] && palabraValidaPorSilabas (w:resto))
                                                            || (silabaValida [x, y] && palabraValidaPorSilabas (z:w:resto)) 
                                                            || (silabaValida [x] && palabraValidaPorSilabas (y:z:w:resto))
palabraValidaPorSilabas (x:y:z:resto) = (silabaValida [x, y, z]   &&      palabraValidaPorSilabas resto)
                                                            || (silabaValida [x, y]     &&      palabraValidaPorSilabas (z:resto))
                                                            || (silabaValida [x]       &&      palabraValidaPorSilabas (y:z:resto))
palabraValidaPorSilabas (x:y:resto) = (silabaValida [x, y]        &&      palabraValidaPorSilabas resto) 
                                                            || (silabaValida [x]   &&      palabraValidaPorSilabas (y:resto))
palabraValidaPorSilabas (x:resto) = silabaValida [x]      &&      palabraValidaPorSilabas resto


--Comprueba si una silaba es valida, supone que las silabas son de maximo 4 letras
silabaValida :: String -> Bool
silabaValida [x] =   let funciones = [esVocal]  -- Solo las vocales pueden ser sílabas solas
                              in all (\f -> f x) funciones
silabaValida str@[_, _] = let funciones = [consonanteFinal]  -- Lista de funciones de reglas de silaba a comprobar TODO
                                      in all (\f -> f str) funciones
silabaValida str@[_, _, _] = let funciones = [consonanteFinal]  -- Lista de funciones de reglas de silaba a comprobar TODO
                                          in all (\f -> f str) funciones
silabaValida str@[_, _, _, _] = let funciones = [consonanteFinal]  -- Lista de funciones de reglas de silaba a comprobar TODO
                                              in all (\f -> f str) funciones
silabaValida _ = False


{-
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
REGLAS GENERALES
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-}

--Devuelve false si encuentra tres o mas consonantes seguidas
noMasTresConsonantesSeguidas :: String -> Bool
noMasTresConsonantesSeguidas str = not (hayTresConsonantesSeguidas str)
  where
    hayTresConsonantesSeguidas [] = False
    hayTresConsonantesSeguidas (x:y:z:xs) | esConsonante x && esConsonante y && esConsonante z = True
                                          | otherwise = hayTresConsonantesSeguidas (y:z:xs)
    hayTresConsonantesSeguidas _ = False


--TODO- hacer mas reglas

{-
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
REGLAS DE SILABAS
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
-}

--Devuelve false si la ultima letra es consonante y distinta de l,n,s,r
consonanteFinal :: String -> Bool
consonanteFinal str = let ult = last str in not(esConsonante ult && not(ult `elem` "lnsr"))

--TODO- hacer mas reglas



























