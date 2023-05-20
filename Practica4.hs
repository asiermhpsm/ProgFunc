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

--
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--MAIN
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--
main :: IO ()
main = do
  putStrLn "Este programa facilita el juego del Scrable. Se pedirá al usuario que ingrese las letras que tiene disponibles, así como las letras disponibles en el tablero y se le devolverá las palabras que puede formar. "
  putStrLn "Tenga en cuenta que de lo que escriba el usuario solo se tendrán en cuenta las letras."
  loop

-- Bucle infinito
loop :: IO ()
loop = do
  putStrLn "\n\nIngrese sus letras (o escriba 'salir' para finalizar):"
  input1 <- getLine
  if input1 /= "salir"
    then do
      putStrLn "Ingrese las letras del tablero:"
      input2 <- getLine
      imprimeRes (soloLetrasMinusculas (input1 ++ input2))
      loop
    else putStrLn "\nSaliendo...."

-- Dado un string con las letras imprime los resultados
imprimeRes :: String -> IO ()
imprimeRes str = do
  let (resultado, n) = generaRes (fromIntegral (length str)) str
  if null resultado
    then putStrLn "No se han podido encontrar palabras"
    else do
      putStrLn "Aquí tienes algunas palabras que puedes usar:"
      putStrLn $ intercalate ", " resultado
      masRes (n - 1) str

-- Genera mas resultados si así lo desea el usuario
masRes :: Integer -> String ->  IO ()
masRes n str = 
  if n == 1
    then return()
    else do
      putStrLn "\n¿Quieres más resultados? (escribe 'si' o 's' si es el caso)"
      input <- getLine
      if input == "si" || input == "s"
        then do
          let (resultado, n') = generaRes n str
          if null resultado
            then putStrLn "\nNo se han podido encontrar mas palabras"
            else do
              putStrLn "\nAquí tienes algunas palabras que puedes usar:"
              putStrLn $ intercalate ", " resultado
              masRes (n' - 1) str
        else return()


--Genera todos los resultados posibles se cierta longitud con la maxima longitud posible
generaRes :: Integer -> String ->  ([String], Integer)
generaRes 1 _ = ([], 1)
generaRes n str = let res = procesarNum n str
                  in if null res
                    then generaRes (n - 1) str
                    else (res, n)


--Dada una lista de letras te da todas las posibles palabras validas de longitud n
procesarNum :: Integer -> String ->  [String]
procesarNum n str = filter palabraValida (generaPalabrasNum n str)

--
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--FUNCIONES AUXILIARES
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--

--Devuelve true si es consonante
esVocal :: Char -> Bool
esVocal c = c `elem` "aeiou"

--Devuelve true si es consonante
esConsonante :: Char -> Bool
esConsonante c = not (esVocal c) && c `elem` ['a'..'z'] 

--Dado un string devuelve solo las letras y convertidas todas a minuscula
soloLetrasMinusculas :: String -> String
soloLetrasMinusculas str = map toLower (filter isAlpha str)



----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--GENERACION DE POSIBLES PALABRAS
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Copiadas las combinaciones de internet (https://www.glc.us.es/~jalonso/vestigium/i1m2014-combinatoria-en-haskell/)

--Genera todas las palabras posibles
generaPalabrasNum :: Integer -> String -> [String]
generaPalabrasNum n str = nub $ permutaLetras(combinacionesNum n str)

--Calcula las permutaciones de todas las palabras de la lista
permutaLetras :: [String] -> [String]
permutaLetras strs = nub $ concatMap (\str -> permutations str) strs

--Devuelve todas las combinaciones posibles de cierta longitud
combinacionesNum :: Integer -> String -> [String]
combinacionesNum = combinaciones_2

-- 2ª definición
combinaciones_2 :: Integer -> [a] -> [[a]]
combinaciones_2 0 _          = [[]]
combinaciones_2 _ []         = []
combinaciones_2 k (x:xs) = 
    [x:ys | ys <- combinaciones_2 (k-1) xs] ++ combinaciones_2 k xs  
 



----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--VALIDAR PALABRA
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--A partir de ahora se supone correcto que el parametro de entrada es una lista de letras en minuscula, sin números ni ningún tipo de simbolo

--Devuelve true si la palabra es valida con las reglas definidas
palabraValida :: String -> Bool
palabraValida str = palabraValidaGeneral str && palabraValidaPorSilabas str

--Realiza comprobaciones generales sobre la palabra y devuelve si es valida 
palabraValidaGeneral :: String -> Bool
palabraValidaGeneral palabra =
  let
    funciones = [noMasTresConsonantesSeguidas, qMasUMasConsonante]  -- Lista de funciones de reglas generales a comprobar TODO
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
silabaValida str@[_, _] = let funciones = [consonanteFinal]  -- Lista de funciones de reglas de silabas de longitud 2 a comprobar TODO
                                      in all (\f -> f str) funciones
silabaValida str@[_, _, _] = let funciones = [consonanteFinal]  -- Lista de funciones de reglas de silabas de longitud 3 a comprobar TODO
                                          in all (\f -> f str) funciones
silabaValida str@[_, _, _, _] = let funciones = [consonanteFinal]  -- Lista de funciones de reglas de silabas de longitud 4 a comprobar TODO
                                              in all (\f -> f str) funciones
silabaValida _ = False


--
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--REGLAS GENERALES
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--

--Devuelve false si encuentra tres o mas consonantes seguidas
noMasTresConsonantesSeguidas :: String -> Bool
noMasTresConsonantesSeguidas str = not (hayTresConsonantesSeguidas str)
  where
    hayTresConsonantesSeguidas [] = False
    hayTresConsonantesSeguidas (x:y:z:xs) | esConsonante x && esConsonante y && esConsonante z = True
                                          | otherwise = hayTresConsonantesSeguidas (y:z:xs)
    hayTresConsonantesSeguidas _ = False

-- Devuelve False si encuentra alguna 'q' que no esté seguida de una 'u' y una vocal
qMasUMasConsonante :: String -> Bool
qMasUMasConsonante [] = True
qMasUMasConsonante [x] = x /= 'q'
qMasUMasConsonante [x, y] = x /= 'q' && qMasUMasConsonante [y]
qMasUMasConsonante (x:y:z:res) = if x == 'q'
                                   then y == 'u' && esVocal z && qMasUMasConsonante (y:z:res)
                                   else qMasUMasConsonante (y:z:res)





--TODO- hacer mas reglas

--
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--REGLAS DE SILABAS
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
--

--Devuelve false si la ultima letra es consonante y distinta de l,n,s,r
consonanteFinal :: String -> Bool
consonanteFinal str = let ult = last str in not(esConsonante ult && not(ult `elem` "lnsr"))

--TODO- hacer mas reglas


